import numpy as np
import matlab

def pyToMatlab(cbaas_val, ty):
  if ty=="Image":
    return matlab.double(a.tolist())
  else:
    return cbaas_val

def matlabToPy(matlab_val, ty):
  if ty=="Image":
    r = matlab_val._data.tolist()
  else:
    return matlab_val

def matlab_run(cbaas_val, do_work, ty_arg, ty_ret):
  mval = pyToMatlab(cbaas_val,ty_arg)
  r    = do_work(mval)
  return matlabToPy(r,ty_ret)
