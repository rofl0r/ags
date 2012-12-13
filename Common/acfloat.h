#ifndef ACFLOAT_H
#define ACFLOAT_H

#ifdef __cplusplus
extern "C" {
#endif

typedef union {
	float f;
	long l;
} longfloat;

long FloatToInt(longfloat fl_value, long roundDirection);
longfloat IntToFloat(long value);
longfloat StringToFloat(const char *theString);
longfloat Math_Cos(longfloat fl_value);
longfloat Math_Sin(longfloat fl_value);
longfloat Math_Tan(longfloat fl_value);
longfloat Math_ArcCos(longfloat fl_value);
longfloat Math_ArcSin(longfloat fl_value);
longfloat Math_ArcTan(longfloat fl_value);
longfloat Math_ArcTan2(longfloat fl_yval, longfloat fl_xval);
longfloat Math_Log(longfloat fl_num);
longfloat Math_Log10(longfloat fl_num);
longfloat Math_Exp(longfloat fl_num);
longfloat Math_Cosh(longfloat fl_num);
longfloat Math_Sinh(longfloat fl_num);
longfloat Math_Tanh(longfloat fl_num);
longfloat Math_RaiseToPower(longfloat fl_base, longfloat fl_exp);
longfloat Math_DegreesToRadians(longfloat fl_value);
longfloat Math_RadiansToDegrees(longfloat fl_value);
longfloat Math_GetPi(void);
longfloat Math_Sqrt(longfloat fl_value);

#ifdef __cplusplus
}
#endif


#endif
