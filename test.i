# This is a comment.
# comments can span multiple lines!
[Mesh]
  type = GeneratedMesh # This is a comment too!
  dim = 2.0
  nx = 0.0
  ny = 1e-3
  xmin = -10
  xmax = 10
  ymin = -10
  ymax = 10
  elem_type = QUAD4
[]

[Variables]
  [x]
    order = FIRST
    family = LAGRANGE
  []
[]

[Materials]
  [const]
    type = GenericConstantMaterial
    block = 0
    prop_names = 'kappa M'
    prop_values = '0.1 1e-3'
  [../]
  [free_energy]
    type = DerivativeParsedMaterial
    block = 0
    f_name = F
    args = 'x'
    function = 'x*log(x)+(1-x)*log(1-x) + x^2*(1-x)^2'
    third_derivatives = false
    enable_jit = true
  [../]
[]

[Kernels]
  [./c_res]
    type = SplitCHParsed
    variable = x
    f_name = F
    kappa_name = kappa
    w = w
  []
  [w_res]
    type = SplitCHWRes
    variable = w
  []
[]
