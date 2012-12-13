// unfortunately MSVC and GCC automatically push floats as doubles
// to functions, thus we need to manually access it as 32-bit
#include <math.h>
#include "acfloat.h"

#if 0
#define SCRIPT_FLOAT(x) long __script_float##x
#define INIT_SCRIPT_FLOAT(x) float x; memcpy(&x, &__script_float##x, sizeof(float))
#define FLOAT_RETURN_TYPE longfloat
#define RETURN_FLOAT(x) long __ret##x; memcpy(&__ret##x, &x, sizeof(float)); return __ret##x
#endif

extern void quit_c(char *);

enum RoundDirections {
	eRoundDown = 0,
	eRoundNearest = 1,
	eRoundUp = 2
};

int FloatToInt(SCRIPT_FLOAT(value), int roundDirection) {
	INIT_SCRIPT_FLOAT(value);

	int intval;

	if (value >= 0.0) {
		if (roundDirection == eRoundDown)
			intval = (int)value;
		else if (roundDirection == eRoundNearest)
			intval = (int)(value + 0.5);
		else if (roundDirection == eRoundUp)
			intval = (int)(value + 0.999999);
		else
			quit_c("!FloatToInt: invalid round direction");
	} else {
		// negative number
		if (roundDirection == eRoundUp)
			intval = (int)value; // this just truncates
		else if (roundDirection == eRoundNearest)
			intval = (int)(value - 0.5);
		else if (roundDirection == eRoundDown)
			intval = (int)(value - 0.999999);
		else
			quit_c("!FloatToInt: invalid round direction");
	}

	return intval;
}

FLOAT_RETURN_TYPE IntToFloat(int value) {
  float fval = value;

  RETURN_FLOAT(fval);
}

FLOAT_RETURN_TYPE StringToFloat(const char *theString) {
  float fval = atof(theString);

  RETURN_FLOAT(fval);
}

FLOAT_RETURN_TYPE Math_Cos(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = cos(value);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Sin(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = sin(value);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Tan(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = tan(value);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_ArcCos(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = acos(value);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_ArcSin(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = asin(value);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_ArcTan(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = atan(value);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_ArcTan2(SCRIPT_FLOAT(yval), SCRIPT_FLOAT(xval)) {
  INIT_SCRIPT_FLOAT(yval);
  INIT_SCRIPT_FLOAT(xval);

  float value = atan2(yval, xval);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Log(SCRIPT_FLOAT(num)) {
  INIT_SCRIPT_FLOAT(num);

  float value = log(num);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Log10(SCRIPT_FLOAT(num)) {
  INIT_SCRIPT_FLOAT(num);

  float value = log10(num); //::

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Exp(SCRIPT_FLOAT(num)) {
  INIT_SCRIPT_FLOAT(num);

  float value = exp(num);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Cosh(SCRIPT_FLOAT(num)) {
  INIT_SCRIPT_FLOAT(num);

  float value = cosh(num);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Sinh(SCRIPT_FLOAT(num)) {
  INIT_SCRIPT_FLOAT(num);

  float value = sinh(num);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_Tanh(SCRIPT_FLOAT(num)) {
  INIT_SCRIPT_FLOAT(num);

  float value = tanh(num);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_RaiseToPower(SCRIPT_FLOAT(base), SCRIPT_FLOAT(exp)) {
  INIT_SCRIPT_FLOAT(base);
  INIT_SCRIPT_FLOAT(exp);

  float value = pow(base, exp); //::

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_DegreesToRadians(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = value * (M_PI / 180.0);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_RadiansToDegrees(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  value = value * (180.0 / M_PI);

  RETURN_FLOAT(value);
}

FLOAT_RETURN_TYPE Math_GetPi(void) {
  float pi = M_PI;

  RETURN_FLOAT(pi);
}

FLOAT_RETURN_TYPE Math_Sqrt(SCRIPT_FLOAT(value)) {
  INIT_SCRIPT_FLOAT(value);

  if (value < 0.0)
    quit_c("!Sqrt: cannot perform square root of negative number");

  value = sqrt(value); // ::

  RETURN_FLOAT(value);
}
