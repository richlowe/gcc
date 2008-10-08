/* cr 6580198 */ 
/* { dg-do compile } */
typedef struct
{
 int x;
 int y;
}
VECTOR;

typedef int int32_t;
typedef unsigned int uint32_t;

typedef struct
{
 VECTOR mvs[4];

 short int pred_values[6][15];
 int acpred_directions[6];

 int mode;
 int quant;
 int32_t sad8[4];
 int32_t sad16;

 int dquant;
 int cbp;

 VECTOR b_mvs[4];
}
MACROBLOCK;

static __inline VECTOR
ChoosePred(const MACROBLOCK * const pMB, const uint32_t mode)
{
 return (mode == 3 ? pMB->mvs[0] : pMB->b_mvs[0]);
}

static void __inline
PreparePredictionsBF(VECTOR * const pmv, const int x, const int y,
       const uint32_t iWcount,
       const MACROBLOCK * const pMB,
       const uint32_t mode_curr,
       const VECTOR hint)
{
  pmv[3] = ChoosePred(pMB+1-iWcount, mode_curr);
}


void
SearchBF_initial(const int x, const int y,
   const uint32_t MotionFlags,
   const uint32_t iFcode,
   MACROBLOCK * const pMB,
   const VECTOR * const predMV,
   int32_t * const best_sad,
   const int32_t mode_current,
   VECTOR hint)
{

 int i;
 VECTOR pmv[7];

 PreparePredictionsBF(pmv, x, y, 0, pMB, mode_current, hint);
}
