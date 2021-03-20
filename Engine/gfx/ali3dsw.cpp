//=============================================================================
//
// Adventure Game Studio (AGS)
//
// Copyright (C) 1999-2011 Chris Jones and 2011-20xx others
// The full list of copyright holders can be found in the Copyright.txt
// file, which is part of this source code distribution.
//
// The AGS source code is provided under the Artistic License 2.0.
// A copy of this license can be found in the file License.txt and at
// http://www.opensource.org/licenses/artistic-license-2.0.php
//
//=============================================================================
#include "gfx/ali3dsw.h"
#include <algorithm>
#include "ac/sys_events.h"
#include "gfx/ali3dexception.h"
#include "gfx/gfxfilter_sdl_renderer.h"
#include "gfx/gfx_util.h"
#include "platform/base/agsplatformdriver.h"
#include "platform/base/sys_main.h"
#include "ac/timer.h"

namespace AGS
{
namespace Engine
{
namespace ALSW
{

using namespace Common;

static unsigned long _trans_alpha_blender32(unsigned long x, unsigned long y, unsigned long n);
RGB faded_out_palette[256];


// ----------------------------------------------------------------------------
// SDLRendererGraphicsDriver
// ----------------------------------------------------------------------------

#if SDL_VERSION_ATLEAST(2, 0, 5)
// used to add alpha back to rendered screen.
static auto fix_alpha_blender = SDL_ComposeCustomBlendMode(
    SDL_BLENDFACTOR_ZERO,
    SDL_BLENDFACTOR_ONE,
    SDL_BLENDOPERATION_ADD,
    SDL_BLENDFACTOR_ONE,
    SDL_BLENDFACTOR_ZERO,
    SDL_BLENDOPERATION_ADD
);
#endif

SDLRendererGraphicsDriver::SDLRendererGraphicsDriver()
{
  _tint_red = 0;
  _tint_green = 0;
  _tint_blue = 0;
  _origVirtualScreen = nullptr;
  virtualScreen = nullptr;
  _stageVirtualScreen = nullptr;

  // Initialize default sprite batch, it will be used when no other batch was activated
  SDLRendererGraphicsDriver::InitSpriteBatch(0, _spriteBatchDesc[0]);
}

bool SDLRendererGraphicsDriver::IsModeSupported(const DisplayMode &mode)
{
  if (mode.Width <= 0 || mode.Height <= 0)
  {
    SDL_SetError("Invalid resolution parameters: %d x %d", mode.Width, mode.Height);
    return false;
  }
  if (mode.ColorDepth != 32) {
    SDL_SetError("Display colour depth not supported: %d", mode.ColorDepth);
    return false;
  }
  return true;
}

int SDLRendererGraphicsDriver::GetDisplayDepthForNativeDepth(int native_color_depth) const
{
    // TODO: check for device caps to know which depth is supported?
    if (native_color_depth > 8)
        return 32;
    return native_color_depth;
}

IGfxModeList *SDLRendererGraphicsDriver::GetSupportedModeList(int color_depth)
{
    std::vector<DisplayMode> modes;
    sys_get_desktop_modes(modes);
    return new SDLRendererGfxModeList(modes);
}

PGfxFilter SDLRendererGraphicsDriver::GetGraphicsFilter() const
{
    return _filter;
}

void SDLRendererGraphicsDriver::SetGraphicsFilter(PSDLRenderFilter filter)
{
  _filter = filter;
  OnSetFilter();

  // TODO: support separate nearest and linear filters, initialize hint by calls to filter object
  // e.g like D3D and OGL filters act
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "nearest");
  // SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear");  // make the scaled rendering look smoother.

  // If we already have a gfx mode set, then use the new filter to update virtual screen immediately
  CreateVirtualScreen();
}

void SDLRendererGraphicsDriver::SetTintMethod(TintMethod method) 
{
  // TODO: support new D3D-style tint method
}

bool SDLRendererGraphicsDriver::SetDisplayMode(const DisplayMode &mode, volatile int *loopTimer)
{
  ReleaseDisplayMode();

  set_color_depth(mode.ColorDepth);

  if (_initGfxCallback != nullptr)
    _initGfxCallback(nullptr);

  if (!IsModeSupported(mode))
    return false;

  if (sys_get_window() == nullptr)
  {
    SDL_Window *window = sys_window_create("", mode.Width, mode.Height, mode.Windowed);

    _hasGamma = SDL_GetWindowGammaRamp(window, _defaultGammaRed, _defaultGammaGreen, _defaultGammaBlue) == 0;

    Uint32 rendererFlags = SDL_RENDERER_ACCELERATED;
    if (mode.Vsync) {
        rendererFlags |= SDL_RENDERER_PRESENTVSYNC;
    }
    _renderer = SDL_CreateRenderer(window, -1, rendererFlags);

    SDL_RendererInfo rinfo{};
    if (SDL_GetRendererInfo(_renderer, &rinfo) == 0) {
      Debug::Printf("Created Renderer: %s", rinfo.name);
      Debug::Printf("Available texture formats:");
      for (int i = 0; i < rinfo.num_texture_formats; i++) {
        Debug::Printf("\t- %s", SDL_GetPixelFormatName(rinfo.texture_formats[i]));
      }
    }
  }
  else
  {
    sys_window_set_style(mode.Windowed);
    if (mode.Windowed)
      sys_window_set_size(mode.Width, mode.Height, true);
  }

  OnInit(loopTimer);
  OnModeSet(mode);
  // If we already have a gfx filter, then use it to update virtual screen immediately
  CreateVirtualScreen();
  return true;
}

void SDLRendererGraphicsDriver::CreateVirtualScreen()
{
  if (!IsModeSet() || !IsRenderFrameValid() || !IsNativeSizeValid() || !_filter)
    return;
  DestroyVirtualScreen();
  // Initialize virtual screen; size is equal to native resolution
  const int vscreen_w = _srcRect.GetWidth();
  const int vscreen_h = _srcRect.GetHeight();
  _origVirtualScreen.reset(new Bitmap(vscreen_w, vscreen_h));
  virtualScreen = _origVirtualScreen.get();
  _stageVirtualScreen = virtualScreen;

  _screenTex = SDL_CreateTexture(_renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, vscreen_w, vscreen_h);

  // Fake bitmap that will wrap over texture pixels for simplier conversion
  _fakeTexBitmap = reinterpret_cast<BITMAP*>(new char[sizeof(BITMAP) + (sizeof(char *) * vscreen_h)]);
  _fakeTexBitmap->w = vscreen_w;
  _fakeTexBitmap->cr = vscreen_w;
  _fakeTexBitmap->h = vscreen_h;
  _fakeTexBitmap->cb = vscreen_h;
  _fakeTexBitmap->clip = true;
  _fakeTexBitmap->cl = 0;
  _fakeTexBitmap->ct = 0;
  _fakeTexBitmap->id = 0;
  _fakeTexBitmap->extra = nullptr;
  _fakeTexBitmap->x_ofs = 0;
  _fakeTexBitmap->y_ofs = 0;
  _fakeTexBitmap->dat = nullptr;

  auto tmpbitmap = create_bitmap_ex(32, 1, 1);
  _fakeTexBitmap->vtable = tmpbitmap->vtable;
  _fakeTexBitmap->write_bank = tmpbitmap->write_bank;
  _fakeTexBitmap->read_bank = tmpbitmap->read_bank;
  destroy_bitmap(tmpbitmap);

  _lastTexPixels = nullptr;
  _lastTexPitch = -1;
}

void SDLRendererGraphicsDriver::DestroyVirtualScreen()
{
  delete[] _fakeTexBitmap; // don't use destroy_bitmap(), because it's a fake structure
  _fakeTexBitmap = nullptr;
  SDL_DestroyTexture(_screenTex);
  _screenTex = nullptr;

  _origVirtualScreen.reset();
  virtualScreen = nullptr;
  _stageVirtualScreen = nullptr;
}

void SDLRendererGraphicsDriver::ReleaseDisplayMode()
{
  OnModeReleased();
  ClearDrawLists();

  DestroyVirtualScreen();
}

bool SDLRendererGraphicsDriver::SetNativeSize(const Size &src_size)
{
  OnSetNativeSize(src_size);
  // If we already have a gfx mode and gfx filter set, then use it to update virtual screen immediately
  CreateVirtualScreen();
  return !_srcRect.IsEmpty();
}

bool SDLRendererGraphicsDriver::SetRenderFrame(const Rect &dst_rect)
{
  OnSetRenderFrame(dst_rect);
  // If we already have a gfx mode and gfx filter set, then use it to update virtual screen immediately
  CreateVirtualScreen();
  return !_dstRect.IsEmpty();
}

void SDLRendererGraphicsDriver::ClearRectangle(int x1, int y1, int x2, int y2, RGB *colorToUse)
{
  // TODO: but maybe is not necessary, as we use SDL_Renderer with accelerated gfx here?
  // See SDL_RenderDrawRect
}

SDLRendererGraphicsDriver::~SDLRendererGraphicsDriver()
{
  SDLRendererGraphicsDriver::UnInit();
}

void SDLRendererGraphicsDriver::UnInit()
{
  OnUnInit();
  ReleaseDisplayMode();

  sys_window_destroy();
}

bool SDLRendererGraphicsDriver::SupportsGammaControl() 
{
  return _hasGamma;
}

void SDLRendererGraphicsDriver::SetGamma(int newGamma)
{
  if (!_hasGamma) { return; }

  Uint16 gamma_red[256];
  Uint16 gamma_green[256];
  Uint16 gamma_blue[256];

  for (int i = 0; i < 256; i++) {
    gamma_red[i] = std::min(((int)_defaultGammaRed[i] * newGamma) / 100, 0xffff);
    gamma_green[i] = std::min(((int)_defaultGammaGreen[i] * newGamma) / 100, 0xffff);
    gamma_blue[i] = std::min(((int)_defaultGammaBlue[i] * newGamma) / 100, 0xffff);
  }

  SDL_SetWindowGammaRamp(sys_get_window(), gamma_red, gamma_green, gamma_blue);
}

int SDLRendererGraphicsDriver::GetCompatibleBitmapFormat(int color_depth)
{
  return color_depth;
}

IDriverDependantBitmap* SDLRendererGraphicsDriver::CreateDDBFromBitmap(Bitmap *bitmap, bool hasAlpha, bool opaque)
{
  ALSoftwareBitmap* newBitmap = new ALSoftwareBitmap(bitmap, opaque, hasAlpha);
  return newBitmap;
}

void SDLRendererGraphicsDriver::UpdateDDBFromBitmap(IDriverDependantBitmap* bitmapToUpdate, Bitmap *bitmap, bool hasAlpha)
{
  ALSoftwareBitmap* alSwBmp = (ALSoftwareBitmap*)bitmapToUpdate;
  alSwBmp->_bmp = bitmap;
  alSwBmp->_hasAlpha = hasAlpha;
}

void SDLRendererGraphicsDriver::DestroyDDB(IDriverDependantBitmap* bitmap)
{
  delete bitmap;
}

void SDLRendererGraphicsDriver::InitSpriteBatch(size_t index, const SpriteBatchDesc &desc)
{
    if (_spriteBatches.size() <= index)
        _spriteBatches.resize(index + 1);
    ALSpriteBatch &batch = _spriteBatches[index];
    batch.List.clear();
    // TODO: correct offsets to have pre-scale (source) and post-scale (dest) offsets!
    const int src_w = desc.Viewport.GetWidth() / desc.Transform.ScaleX;
    const int src_h = desc.Viewport.GetHeight() / desc.Transform.ScaleY;
    // Surface was prepared externally (common for room cameras)
    if (desc.Surface != nullptr)
    {
        batch.Surface = std::static_pointer_cast<Bitmap>(desc.Surface);
        batch.Opaque = true;
        batch.IsVirtualScreen = false;
    }
    // In case something was not initialized
    else if (desc.Viewport.IsEmpty() || !virtualScreen)
    {
        batch.Surface.reset();
        batch.Opaque = false;
        batch.IsVirtualScreen = false;
    }
    // Drawing directly on a viewport without transformation (other than offset)
    else if (desc.Transform.ScaleX == 1.f && desc.Transform.ScaleY == 1.f)
    {
        // We need this subbitmap for plugins, which use _stageVirtualScreen and are unaware of possible multiple viewports;
        // TODO: there could be ways to optimize this further, but best is to update plugin rendering hooks (and upgrade plugins)
        if (!batch.Surface || !batch.IsVirtualScreen || batch.Surface->GetWidth() != src_w || batch.Surface->GetHeight() != src_h
            || batch.Surface->GetSubOffset() != desc.Viewport.GetLT())
        {
            Rect rc = RectWH(desc.Viewport.Left, desc.Viewport.Top, desc.Viewport.GetWidth(), desc.Viewport.GetHeight());
            batch.Surface.reset(BitmapHelper::CreateSubBitmap(virtualScreen, rc));
        }
        batch.Opaque = true;
        batch.IsVirtualScreen = true;
    }
    // No surface prepared and has transformation other than offset
    else if (!batch.Surface || batch.IsVirtualScreen || batch.Surface->GetWidth() != src_w || batch.Surface->GetHeight() != src_h)
    {
        batch.Surface.reset(new Bitmap(src_w, src_h));
        batch.Opaque = false;
        batch.IsVirtualScreen = false;
    }
}

void SDLRendererGraphicsDriver::ResetAllBatches()
{
    for (ALSpriteBatches::iterator it = _spriteBatches.begin(); it != _spriteBatches.end(); ++it)
        it->List.clear();
}

void SDLRendererGraphicsDriver::DrawSprite(int x, int y, IDriverDependantBitmap* bitmap)
{
    _spriteBatches[_actSpriteBatch].List.push_back(ALDrawListEntry((ALSoftwareBitmap*)bitmap, x, y));
}

void SDLRendererGraphicsDriver::SetScreenFade(int red, int green, int blue)
{
    // TODO: was not necessary atm
    // TODO: checkme later
}

void SDLRendererGraphicsDriver::SetScreenTint(int red, int green, int blue)
{
    _tint_red = red; _tint_green = green; _tint_blue = blue;
    if (((_tint_red > 0) || (_tint_green > 0) || (_tint_blue > 0)) && (_mode.ColorDepth > 8))
    {
      _spriteBatches[_actSpriteBatch].List.push_back(ALDrawListEntry((ALSoftwareBitmap*)0x1, 0, 0));
    }
}

void SDLRendererGraphicsDriver::RenderToBackBuffer()
{
    // Render all the sprite batches with necessary transformations
    //
    // NOTE: that's not immediately clear whether it would be faster to first draw upon a camera-sized
    // surface then stretch final result to the viewport on screen, or stretch-blit each individual
    // sprite right onto screen bitmap. We'd need to do proper profiling to know that.
    // An important thing is that Allegro does not provide stretching functions for drawing sprites
    // with blending and translucency; it seems you'd have to first stretch the original sprite onto a
    // temp buffer and then TransBlendBlt / LitBlendBlt it to the final destination. Of course, doing
    // that here would slow things down significantly, so if we ever go that way sprite caching will
    // be required (similarily to how AGS caches flipped/scaled object sprites now for).
    //
    for (size_t i = 0; i <= _actSpriteBatch; ++i)
    {
        const Rect &viewport = _spriteBatchDesc[i].Viewport;
        const SpriteTransform &transform = _spriteBatchDesc[i].Transform;
        const ALSpriteBatch &batch = _spriteBatches[i];

        virtualScreen->SetClip(viewport);
        Bitmap *surface = batch.Surface.get();
        const int view_offx = viewport.Left;
        const int view_offy = viewport.Top;
        if (surface)
        {
            if (!batch.Opaque)
                surface->ClearTransparent();
            _stageVirtualScreen = surface;
            RenderSpriteBatch(batch, surface, transform.X, transform.Y);
            if (!batch.IsVirtualScreen)
                virtualScreen->StretchBlt(surface, RectWH(view_offx, view_offy, viewport.GetWidth(), viewport.GetHeight()),
                    batch.Opaque ? kBitmap_Copy : kBitmap_Transparency);
        }
        else
        {
            RenderSpriteBatch(batch, virtualScreen, view_offx + transform.X, view_offy + transform.Y);
        }
        _stageVirtualScreen = virtualScreen;
    }
    ClearDrawLists();
}

void SDLRendererGraphicsDriver::RenderSpriteBatch(const ALSpriteBatch &batch, Common::Bitmap *surface, int surf_offx, int surf_offy)
{
  const std::vector<ALDrawListEntry> &drawlist = batch.List;
  for (size_t i = 0; i < drawlist.size(); i++)
  {
    if (drawlist[i].bitmap == nullptr)
    {
      if (_nullSpriteCallback)
        _nullSpriteCallback(drawlist[i].x, drawlist[i].y);
      else
        throw Ali3DException("Unhandled attempt to draw null sprite");

      continue;
    }
    else if (drawlist[i].bitmap == (ALSoftwareBitmap*)0x1)
    {
      // draw screen tint fx
      set_trans_blender(_tint_red, _tint_green, _tint_blue, 0);
      surface->LitBlendBlt(surface, 0, 0, 128);
      continue;
    }

    ALSoftwareBitmap* bitmap = drawlist[i].bitmap;
    int drawAtX = drawlist[i].x + surf_offx;
    int drawAtY = drawlist[i].y + surf_offy;

    if (bitmap->_transparency >= 255) {} // fully transparent, do nothing
    else if ((bitmap->_opaque) && (bitmap->_bmp == surface) && (bitmap->_transparency == 0)) {}
    else if (bitmap->_opaque)
    {
        surface->Blit(bitmap->_bmp, 0, 0, drawAtX, drawAtY, bitmap->_bmp->GetWidth(), bitmap->_bmp->GetHeight());
        // TODO: we need to also support non-masked translucent blend, but...
        // Allegro 4 **does not have such function ready** :( (only masked blends, where it skips magenta pixels);
        // I am leaving this problem for the future, as coincidentally software mode does not need this atm.
    }
    else if (bitmap->_hasAlpha)
    {
      if (bitmap->_transparency == 0) // no global transparency, simple alpha blend
        set_alpha_blender();
      else
        // here _transparency is used as alpha (between 1 and 254)
        set_blender_mode(nullptr, nullptr, _trans_alpha_blender32, 0, 0, 0, bitmap->_transparency);

      surface->TransBlendBlt(bitmap->_bmp, drawAtX, drawAtY);
    }
    else
    {
      // here _transparency is used as alpha (between 1 and 254), but 0 means opaque!
      GfxUtil::DrawSpriteWithTransparency(surface, bitmap->_bmp, drawAtX, drawAtY,
          bitmap->_transparency ? bitmap->_transparency : 255);
    }
  }
}

void SDLRendererGraphicsDriver::BlitToTexture()
{
    void *pixels = nullptr;
    int pitch = 0;
    auto res = SDL_LockTexture(_screenTex, NULL, &pixels, &pitch);
    if (res != 0) { return; }

    // Because the virtual screen may be of any color depth,
    // we wrap texture pixels in a fake bitmap here and call
    // standard blit operation, for simplicity sake.
    const int vwidth = virtualScreen->GetWidth();
    const int vheight = virtualScreen->GetHeight();
    if ((_lastTexPixels != pixels) || (_lastTexPitch != pitch)) {
        _fakeTexBitmap->dat = pixels;
        auto p = (unsigned char *)pixels;
        for (int i = 0; i < vheight; i++) {
            _fakeTexBitmap->line[i] = p;
            p += pitch;
        }
        _lastTexPixels = (unsigned char *)pixels;
        _lastTexPitch = pitch;
    }

    blit(virtualScreen->GetAllegroBitmap(), _fakeTexBitmap, 0, 0, 0, 0, vwidth, vheight);

    SDL_UnlockTexture(_screenTex);
}

void SDLRendererGraphicsDriver::Present()
{
    if (!_renderer) { return; }

    BlitToTexture();

    SDL_SetRenderDrawBlendMode(_renderer, SDL_BLENDMODE_NONE);
    SDL_SetRenderDrawColor(_renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderFillRect(_renderer, nullptr);

    SDL_Rect dst;
    dst.x = _dstRect.Left;
    dst.y = _dstRect.Top;
    dst.w = _dstRect.GetWidth();
    dst.h = _dstRect.GetHeight();
    SDL_RenderCopyEx(_renderer, _screenTex, nullptr, &dst, 0.0, nullptr, _renderFlip);

    SDL_RenderPresent(_renderer);
}

void SDLRendererGraphicsDriver::Render(int /*xoff*/, int /*yoff*/, GlobalFlipType flip)
{
  switch (flip) {
    case kFlip_Both: _renderFlip = (SDL_RendererFlip)(SDL_FLIP_HORIZONTAL | SDL_FLIP_VERTICAL); break;
    case kFlip_Horizontal: _renderFlip = SDL_FLIP_HORIZONTAL; break;
    case kFlip_Vertical: _renderFlip = SDL_FLIP_VERTICAL; break;
    default: _renderFlip = SDL_FLIP_NONE; break;
  }

  RenderToBackBuffer();
  Present();
}

void SDLRendererGraphicsDriver::Render()
{
  Render(0, 0, kFlip_None);
}

void SDLRendererGraphicsDriver::Vsync()
{
}

Bitmap *SDLRendererGraphicsDriver::GetMemoryBackBuffer()
{
  return virtualScreen;
}

void SDLRendererGraphicsDriver::SetMemoryBackBuffer(Bitmap *backBuffer)
{
  if (backBuffer)
  {
    virtualScreen = backBuffer;
  }
  else
  {
    virtualScreen = _origVirtualScreen.get();
  }
  _stageVirtualScreen = virtualScreen;

  // Reset old virtual screen's subbitmaps
  for (auto &batch : _spriteBatches)
  {
    if (batch.IsVirtualScreen)
      batch.Surface.reset();
  }
}

Bitmap *SDLRendererGraphicsDriver::GetStageBackBuffer()
{
    return _stageVirtualScreen;
}

bool SDLRendererGraphicsDriver::GetCopyOfScreenIntoBitmap(Bitmap *destination, bool at_native_res, GraphicResolution *want_fmt)
{
  (void)at_native_res; // software driver always renders at native resolution at the moment
  // software filter is taught to copy to any size
  if (destination->GetColorDepth() != _mode.ColorDepth)
  {
    if (want_fmt)
        *want_fmt = GraphicResolution(destination->GetWidth(), destination->GetHeight(), _mode.ColorDepth);
    return false;
  }

  if (destination->GetSize() == virtualScreen->GetSize())
  {
    destination->Blit(virtualScreen, 0, 0, 0, 0, virtualScreen->GetWidth(), virtualScreen->GetHeight());
  }
  else
  {
    destination->StretchBlt(virtualScreen,
          RectWH(0, 0, virtualScreen->GetWidth(), virtualScreen->GetHeight()),
          RectWH(0, 0, destination->GetWidth(), destination->GetHeight()));
  }
  return true;
}

/**
	fade.c - High Color Fading Routines

	Last Revision: 21 June, 2002

	Author: Matthew Leverton
**/
void SDLRendererGraphicsDriver::highcolor_fade_in(Bitmap *vs, void(*draw_callback)(), int offx, int offy, int speed, int targetColourRed, int targetColourGreen, int targetColourBlue)
{
   Bitmap *bmp_orig = vs;
   const int col_depth = bmp_orig->GetColorDepth();
   const int clearColor = makecol_depth(col_depth, targetColourRed, targetColourGreen, targetColourBlue);
   if (speed <= 0) speed = 16;

   Bitmap *bmp_buff = new Bitmap(bmp_orig->GetWidth(), bmp_orig->GetHeight(), col_depth);
   SetMemoryBackBuffer(bmp_buff);
   for (int a = 0; a < 256; a+=speed)
   {
       bmp_buff->Fill(clearColor);
       set_trans_blender(0,0,0,a);
       bmp_buff->TransBlendBlt(bmp_orig, 0, 0);
       
       if (draw_callback)
           draw_callback();
       RenderToBackBuffer();
       Present();

       sys_evt_process_pending();
       if (_pollingCallback)
          _pollingCallback();

       WaitForNextFrame();
   }
   delete bmp_buff;

   SetMemoryBackBuffer(vs);
   if (draw_callback)
       draw_callback();
   RenderToBackBuffer();
   Present();
}

void SDLRendererGraphicsDriver::highcolor_fade_out(Bitmap *vs, void(*draw_callback)(), int offx, int offy, int speed, int targetColourRed, int targetColourGreen, int targetColourBlue)
{
    Bitmap *bmp_orig = vs;
    const int col_depth = vs->GetColorDepth();
    const int clearColor = makecol_depth(col_depth, targetColourRed, targetColourGreen, targetColourBlue);
    if (speed <= 0) speed = 16;

    Bitmap *bmp_buff = new Bitmap(bmp_orig->GetWidth(), bmp_orig->GetHeight(), col_depth);
    SetMemoryBackBuffer(bmp_buff);
    for (int a = 255 - speed; a > 0; a -= speed)
    {
        bmp_buff->Fill(clearColor);
        set_trans_blender(0, 0, 0, a);
        bmp_buff->TransBlendBlt(bmp_orig, 0, 0);

        if (draw_callback)
            draw_callback();
        RenderToBackBuffer();
        Present();

        sys_evt_process_pending();
        if (_pollingCallback)
            _pollingCallback();

        WaitForNextFrame();
    }
    delete bmp_buff;

    SetMemoryBackBuffer(vs);
    vs->Clear(clearColor);
    if (draw_callback)
        draw_callback();
    RenderToBackBuffer();
    Present();
}
/** END FADE.C **/

// palette fading routiens
// from allegro, modified for mp3
void initialize_fade_256(int r, int g, int b) {
  int a;
  for (a = 0; a < 256; a++) {
    faded_out_palette[a].r = r / 4;
	  faded_out_palette[a].g = g / 4;
	  faded_out_palette[a].b = b / 4;
  }
}

void SDLRendererGraphicsDriver::__fade_from_range(PALETTE source, PALETTE dest, int speed, int from, int to) 
{
   PALETTE temp;
   int c;

   for (c=0; c<PAL_SIZE; c++)
      temp[c] = source[c];

   for (c=0; c<64; c+=speed) {
      fade_interpolate(source, dest, temp, c, from, to);
      set_palette_range(temp, from, to, TRUE);

      RenderToBackBuffer();
      Present();

      sys_evt_process_pending();
      if (_pollingCallback)
          _pollingCallback();
   }

   set_palette_range(dest, from, to, TRUE);
}

void SDLRendererGraphicsDriver::__fade_out_range(int speed, int from, int to, int targetColourRed, int targetColourGreen, int targetColourBlue) 
{
   PALETTE temp;

   initialize_fade_256(targetColourRed, targetColourGreen, targetColourBlue);
   get_palette(temp);
   __fade_from_range(temp, faded_out_palette, speed, from, to);
}

void SDLRendererGraphicsDriver::FadeOut(int speed, int targetColourRed, int targetColourGreen, int targetColourBlue) {
  if (_mode.ColorDepth > 8) 
  {
    highcolor_fade_out(virtualScreen, _drawPostScreenCallback, 0, 0, speed * 4, targetColourRed, targetColourGreen, targetColourBlue);
  }
  else
  {
    __fade_out_range(speed, 0, 255, targetColourRed, targetColourGreen, targetColourBlue);
  }
}

void SDLRendererGraphicsDriver::FadeIn(int speed, PALETTE p, int targetColourRed, int targetColourGreen, int targetColourBlue) {
  if (_drawScreenCallback)
  {
    _drawScreenCallback();
    RenderToBackBuffer();
  }
  if (_mode.ColorDepth > 8)
  {
    highcolor_fade_in(virtualScreen, _drawPostScreenCallback, 0, 0, speed * 4, targetColourRed, targetColourGreen, targetColourBlue);
  }
  else
  {
	initialize_fade_256(targetColourRed, targetColourGreen, targetColourBlue);
	__fade_from_range(faded_out_palette, p, speed, 0,255);
  }
}

void SDLRendererGraphicsDriver::BoxOutEffect(bool blackingOut, int speed, int delay)
{
  if (blackingOut)
  {
    int yspeed = _srcRect.GetHeight() / (_srcRect.GetWidth() / speed);
    int boxwid = speed, boxhit = yspeed;
    Bitmap *bmp_orig = virtualScreen;
    Bitmap *bmp_buff = new Bitmap(bmp_orig->GetWidth(), bmp_orig->GetHeight(), bmp_orig->GetColorDepth());
    SetMemoryBackBuffer(bmp_buff);

    while (boxwid < _srcRect.GetWidth()) {
      boxwid += speed;
      boxhit += yspeed;
      int vcentre = _srcRect.GetHeight() / 2;
      bmp_orig->FillRect(Rect(_srcRect.GetWidth() / 2 - boxwid / 2, vcentre - boxhit / 2,
          _srcRect.GetWidth() / 2 + boxwid / 2, vcentre + boxhit / 2), 0);
      bmp_buff->Fill(0);
      bmp_buff->Blit(bmp_orig);

      if (_drawPostScreenCallback)
          _drawPostScreenCallback();
      RenderToBackBuffer();
      Present();

      sys_evt_process_pending();
      if (_pollingCallback)
          _pollingCallback();

      platform->Delay(delay);
    }
    delete bmp_buff;
    SetMemoryBackBuffer(bmp_orig);
  }
  else
  {
    throw Ali3DException("BoxOut fade-in not implemented in sw gfx driver");
  }
}
// end fading routines

// add the alpha values together, used for compositing alpha images
// TODO: why is this here, move to gfx/blender? check if there's already similar function there
static unsigned long _trans_alpha_blender32(unsigned long x, unsigned long y, unsigned long n)
{
   unsigned long res, g;

   n = (n * geta32(x)) / 256;

   if (n)
      n++;

   res = ((x & 0xFF00FF) - (y & 0xFF00FF)) * n / 256 + y;
   y &= 0xFF00;
   x &= 0xFF00;
   g = (x - y) * n / 256 + y;

   res &= 0xFF00FF;
   g &= 0xFF00;

   return res | g;
}


SDLRendererGraphicsFactory *SDLRendererGraphicsFactory::_factory = nullptr;

SDLRendererGraphicsFactory::~SDLRendererGraphicsFactory()
{
    _factory = nullptr;
}

size_t SDLRendererGraphicsFactory::GetFilterCount() const
{
    return 1;
}

const GfxFilterInfo *SDLRendererGraphicsFactory::GetFilterInfo(size_t index) const
{
    switch (index)
    {
    case 0:
        return &SDLRendererGfxFilter::FilterInfo;
    default:
        return nullptr;
    }
}

String SDLRendererGraphicsFactory::GetDefaultFilterID() const
{
    return SDLRendererGfxFilter::FilterInfo.Id;
}

/* static */ SDLRendererGraphicsFactory *SDLRendererGraphicsFactory::GetFactory()
{
    if (!_factory)
        _factory = new SDLRendererGraphicsFactory();
    return _factory;
}

SDLRendererGraphicsDriver *SDLRendererGraphicsFactory::EnsureDriverCreated()
{
    if (!_driver)
        _driver = new SDLRendererGraphicsDriver();
    return _driver;
}

SDLRendererGfxFilter *SDLRendererGraphicsFactory::CreateFilter(const String &id)
{
    if (SDLRendererGfxFilter::FilterInfo.Id.CompareNoCase(id) == 0)
        return new SDLRendererGfxFilter();
    return nullptr;
}

} // namespace ALSW
} // namespace Engine
} // namespace AGS
