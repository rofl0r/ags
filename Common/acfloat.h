#ifndef ACFLOAT_H
#define ACFLOAT_H

#ifdef __cplusplus
extern "C" {
#endif

typedef union {
	float f;
	long l;
} longfloat;

#define RETURN_FLOAT(x) return (longfloat){.f = (x)}
#define SCRIPT_FLOAT(x) longfloat __script_float##x
#define INIT_SCRIPT_FLOAT(x) float x = __script_float##x.f
#define FLOAT_RETURN_TYPE longfloat

int FloatToInt(SCRIPT_FLOAT(value), int roundDirection);
FLOAT_RETURN_TYPE IntToFloat(int value);
FLOAT_RETURN_TYPE StringToFloat(const char *theString);
FLOAT_RETURN_TYPE Math_Cos(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_Sin(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_Tan(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_ArcCos(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_ArcSin(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_ArcTan(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_ArcTan2(SCRIPT_FLOAT(yval), SCRIPT_FLOAT(xval));
FLOAT_RETURN_TYPE Math_Log(SCRIPT_FLOAT(num));
FLOAT_RETURN_TYPE Math_Log10(SCRIPT_FLOAT(num));
FLOAT_RETURN_TYPE Math_Exp(SCRIPT_FLOAT(num));
FLOAT_RETURN_TYPE Math_Cosh(SCRIPT_FLOAT(num));
FLOAT_RETURN_TYPE Math_Sinh(SCRIPT_FLOAT(num));
FLOAT_RETURN_TYPE Math_Tanh(SCRIPT_FLOAT(num));
FLOAT_RETURN_TYPE Math_RaiseToPower(SCRIPT_FLOAT(base), SCRIPT_FLOAT(exp));
FLOAT_RETURN_TYPE Math_DegreesToRadians(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_RadiansToDegrees(SCRIPT_FLOAT(value));
FLOAT_RETURN_TYPE Math_GetPi(void);
FLOAT_RETURN_TYPE Math_Sqrt(SCRIPT_FLOAT(value));

#ifdef __cplusplus
}
#endif


#endif
