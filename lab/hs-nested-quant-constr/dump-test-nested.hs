checkFamInstConsistency [Prelude]
Tc2 (src)
Tc3
tcExtendKindEnvList []
tcExtendKindEnvList []
---- tcTyClGroup ---- {
Decls for [Pair]
tcExtendKindEnv
  [rsb :-> APromotionErr TyConPE, rsc :-> APromotionErr RecDataConPE]
---- kcTyClGroup ---- {
  module Foo
  data Pair a_a1bf b_a1bg = MkPair a_a1bf b_a1bg
getInitialKinds {
getInitialKinds done }
tcExtendKindEnvList []
solveEqualities { level = 1
getInitialKinds {
bindImplicitTKBndrs
  []
  []
tc_extend_local_env []
tcExtendBinderStack []
bindExplicTKBndrs [a_a1bf, b_a1bg]
newMetaKindVar k_a1bl[tau:1]
newTyVarTyVar a_a1bm[tyv:1]
tc_extend_local_env
  [(a_a1bf, Type variable ‘a_a1bf’ = a_a1bm[tyv:1] :: k_a1bl[tau:1])]
tcExtendBinderStack [a_a1bf a_a1bm[tyv:1]]
newMetaKindVar k_a1bn[tau:1]
newTyVarTyVar b_a1bo[tyv:1]
tc_extend_local_env
  [(b_a1bg, Type variable ‘b_a1bg’ = b_a1bo[tyv:1] :: k_a1bn[tau:1])]
tcExtendBinderStack [b_a1bg b_a1bo[tyv:1]]
kcLHsQTyVars: not-cusk
  Pair
  []
  [a_a1bf, b_a1bg]
  []
  [a_a1bm[tyv:1], b_a1bo[tyv:1]]
  k_a1bl[tau:1] -> k_a1bn[tau:1] -> *
getInitialKinds done }
kcTyClGroup: initial kinds
  Pair :: k_a1bl[tau:1] -> k_a1bn[tau:1] -> *
tcExtendKindEnvList
  [(Pair, ATcTyCon Pair[tc] :: k_a1bl[tau:1] -> k_a1bn[tau:1] -> *)]
kcTyClDecl { Pair
bindTyClTyVars
  Pair [anon-vis (a_a1bm[tyv:1]), anon-vis (b_a1bo[tyv:1])]
  [(a_a1bf, a_a1bm[tyv:1]), (b_a1bg, b_a1bo[tyv:1])]
tc_extend_local_env
  [(a_a1bf, Type variable ‘a_a1bf’ = a_a1bm[tyv:1] :: k_a1bl[tau:1]),
   (b_a1bg, Type variable ‘b_a1bg’ = b_a1bo[tyv:1] :: k_a1bn[tau:1])]
tcExtendBinderStack [a_a1bf a_a1bm[tyv:1], b_a1bg b_a1bo[tyv:1]]
bindExplicTKBndrs []
kcConDecl {
  MkPair
  PrefixCon [a_a1bf, b_a1bg]
newAnonMetaTyVar t_a1bp[tau:1]
lk1 a_a1bf
tcInferApps {
  a_a1bf
  []
tcInferApps } a_a1bm[tyv:1] :: k_a1bl[tau:1]
checkExpectedKind
  a_a1bm[tyv:1]
  k_a1bl[tau:1]
checkExpectedKindX
  a_a1bf
  act_kind': k_a1bl[tau:1]
  exp_kind: TYPE t_a1bp[tau:1]
u_tys
  tclvl 1
  k_a1bl[tau:1] ~ TYPE t_a1bp[tau:1]
  arising from a type equality k_a1bl[tau:1] ~ TYPE t_a1bp[tau:1]
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    k_a1bl[tau:1] ~ TYPE t_a1bp[tau:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    k_a1bl[tau:1] ~ TYPE t_a1bp[tau:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1bl[tau:1] :: *
  TYPE t_a1bp[tau:1] :: *
  True
  <*>_N
writeMetaTyVar k_a1bl[tau:1] := TYPE t_a1bp[tau:1]
u_tys yields no coercion
checkExpectedKind
  k_a1bl[tau:1]
  TYPE t_a1bp[tau:1]
  <TYPE t_a1bp[tau:1]>_N
newAnonMetaTyVar t_a1bq[tau:1]
lk1 b_a1bg
tcInferApps {
  b_a1bg
  []
tcInferApps } b_a1bo[tyv:1] :: k_a1bn[tau:1]
checkExpectedKind
  b_a1bo[tyv:1]
  k_a1bn[tau:1]
checkExpectedKindX
  b_a1bg
  act_kind': k_a1bn[tau:1]
  exp_kind: TYPE t_a1bq[tau:1]
u_tys
  tclvl 1
  k_a1bn[tau:1] ~ TYPE t_a1bq[tau:1]
  arising from a type equality k_a1bn[tau:1] ~ TYPE t_a1bq[tau:1]
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    k_a1bn[tau:1] ~ TYPE t_a1bq[tau:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    k_a1bn[tau:1] ~ TYPE t_a1bq[tau:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1bn[tau:1] :: *
  TYPE t_a1bq[tau:1] :: *
  True
  <*>_N
writeMetaTyVar k_a1bn[tau:1] := TYPE t_a1bq[tau:1]
u_tys yields no coercion
checkExpectedKind
  k_a1bn[tau:1]
  TYPE t_a1bq[tau:1]
  <TYPE t_a1bq[tau:1]>_N
kcConDecl } MkPair
kcTyClDecl done } Pair
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1br
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1bs
Skolemising a_a1bm[tyv:1] := a_a1bm[sk:1]
writeMetaTyVar a_a1bm[tyv:1] := a_a1bm[sk:1]
Skolemising b_a1bo[tyv:1] := b_a1bo[sk:1]
writeMetaTyVar b_a1bo[tyv:1] := b_a1bo[sk:1]
quantifyTyVars 1
  0
  DV {dv_kvs = {t_a1bp[tau:1], t_a1bq[tau:1]}, dv_tvs = {},
      dv_cvs = {}}
  {}
Defaulting a RuntimeRep var to LiftedRep t_a1bp[tau:1]
writeMetaTyVar t_a1bp[tau:1] := 'GHC.Types.LiftedRep
Defaulting a RuntimeRep var to LiftedRep t_a1bq[tau:1]
writeMetaTyVar t_a1bq[tau:1] := 'GHC.Types.LiftedRep
quantifyTyVars 2
  globals: {}
  mono_tvs: {}
  nondep:
  dep: (t_a1bp[tau:1] :: GHC.Types.RuntimeRep)
       (t_a1bq[tau:1] :: GHC.Types.RuntimeRep)
  dep_kvs'
  nondep_tvs'
generaliseTcTyCon: before zonkRec
  spec_req_tvs = (a_a1bm[sk:1] :: TYPE t_a1bp[tau:1])
                 (b_a1bo[sk:1] :: TYPE t_a1bq[tau:1])
  inferred =
generaliseTcTyCon: post zonk
  tycon = Pair[tc]
  inferred =
  ze = ZE {ze_tv_env = [a1bm :-> a_a1bf, a1bo :-> b_a1bg]
           ze_id_env = []}
  spec_req_prs = [(a_a1bf, a_a1bm[tyv:1]), (b_a1bg, b_a1bo[tyv:1])]
  spec_req_tvs = (a_a1bm[sk:1] :: TYPE t_a1bp[tau:1])
                 (b_a1bo[sk:1] :: TYPE t_a1bq[tau:1])
  final_spec_req_tvs = a_a1bf b_a1bg
generaliseTcTyCon done
  tycon = Pair[tc]
  tc_res_kind = *
  dep_fv_set = {t_a1bp[tau:1], t_a1bq[tau:1]}
  final_spec_req_tvs = a_a1bf b_a1bg
  inferred =
  specified =
  required_tcbs = [anon-vis (a_a1bf), anon-vis (b_a1bg)]
  final_tcbs = [anon-vis (a_a1bf), anon-vis (b_a1bg)]
---- kcTyClGroup end ---- } Pair :: * -> * -> *
tcTyAndCl generalized kinds
  (Pair, [anon-vis (a_a1bf), anon-vis (b_a1bg)], * True)
tcExtendKindEnvList [(Pair, ATcTyCon Pair[tc] :: * -> * -> *)]
---- tcTyClDecl ---- {
  data Pair a_a1bf b_a1bg = MkPair a_a1bf b_a1bg
bindTyClTyVars
  Pair [anon-vis (a_a1bf), anon-vis (b_a1bg)]
  [(a_a1bf, a_a1bf), (b_a1bg, b_a1bg)]
tc_extend_local_env
  [(a_a1bf, Type variable ‘a_a1bf’ = a_a1bf :: *),
   (b_a1bg, Type variable ‘b_a1bg’ = b_a1bg :: *)]
tcExtendBinderStack [a_a1bf a_a1bf, b_a1bg b_a1bg]
solveEqualities { level = 1
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1bu
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1bv
tcConDecl 1
  MkPair
  []
solveEqualities { level = 1
bindExplicTKBndrs []
tcConArg 1 a_a1bf
newAnonMetaTyVar t_a1bw[tau:1]
lk1 a_a1bf
tcInferApps {
  a_a1bf
  []
tcInferApps } a_a1bf :: *
checkExpectedKind
  a_a1bf
  *
checkExpectedKindX
  a_a1bf
  act_kind': *
  exp_kind: TYPE t_a1bw[tau:1]
u_tys
  tclvl 1
  * ~ TYPE t_a1bw[tau:1]
  arising from a type equality * ~ TYPE t_a1bw[tau:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ t_a1bw[tau:1]
  arising from a type equality * ~ TYPE t_a1bw[tau:1]
u_tys
  tclvl 1
  GHC.Types.RuntimeRep ~ GHC.Types.RuntimeRep
  arising from a kind equality arising from
    t_a1bw[tau:1] ~ 'GHC.Types.LiftedRep
u_tys yields no coercion
uUnfilledVar2 ok
  t_a1bw[tau:1] :: GHC.Types.RuntimeRep
  'GHC.Types.LiftedRep :: GHC.Types.RuntimeRep
  True
  <GHC.Types.RuntimeRep>_N
writeMetaTyVar t_a1bw[tau:1] := 'GHC.Types.LiftedRep
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  *
  TYPE t_a1bw[tau:1]
  <*>_N
tcConArg 2 a_a1bf
tcConArg 1 b_a1bg
newAnonMetaTyVar t_a1bx[tau:1]
lk1 b_a1bg
tcInferApps {
  b_a1bg
  []
tcInferApps } b_a1bg :: *
checkExpectedKind
  b_a1bg
  *
checkExpectedKindX
  b_a1bg
  act_kind': *
  exp_kind: TYPE t_a1bx[tau:1]
u_tys
  tclvl 1
  * ~ TYPE t_a1bx[tau:1]
  arising from a type equality * ~ TYPE t_a1bx[tau:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ t_a1bx[tau:1]
  arising from a type equality * ~ TYPE t_a1bx[tau:1]
u_tys
  tclvl 1
  GHC.Types.RuntimeRep ~ GHC.Types.RuntimeRep
  arising from a kind equality arising from
    t_a1bx[tau:1] ~ 'GHC.Types.LiftedRep
u_tys yields no coercion
uUnfilledVar2 ok
  t_a1bx[tau:1] :: GHC.Types.RuntimeRep
  'GHC.Types.LiftedRep :: GHC.Types.RuntimeRep
  True
  <GHC.Types.RuntimeRep>_N
writeMetaTyVar t_a1bx[tau:1] := 'GHC.Types.LiftedRep
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  *
  TYPE t_a1bx[tau:1]
  <*>_N
tcConArg 2 b_a1bg
lookupCF
  MkPair
  Nothing
  []
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1by
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1bz
kindGeneralise1 forall a b. a -> b -> ()
kindGeneralize
  forall a b. a -> b -> ()
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
quantifyTyVars 1
  0
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
  {a_a1bf, b_a1bg}
quantifyTyVars 2
  globals: {a_a1bf, b_a1bg}
  mono_tvs: {a_a1bf, b_a1bg}
  nondep:
  dep:
  dep_kvs'
  nondep_tvs'
tcConDecl 2
  MkPair
  []
tcConDecl 2 MkPair
tcDataDefn
  Pair
  [anon-vis (a_a1bf), anon-vis (b_a1bg)]
  []
---- tcTyClDecl end ---- } Pair
Starting synonym cycle check [Pair]
Done synonym cycle check [Pair]
Starting validity check [Pair]
Starting validity for tycon Pair
checkValidTyCon
  Pair
  Nothing
cvtc1 Pair
cvtc2 Pair
checkValidDataCon
  MkPair
  Pair
  [a_a1bf, b_a1bg]
  Pair a_a1bf b_a1bg :: *
  Pair a_a1bf b_a1bg :: *
checkValidDataCon 2 forall a b. a -> b -> Pair a b
checkValidType forall a b. a -> b -> Pair a b :: *
check_type
  forall a b. a -> b -> Pair a b
  True
done ct forall a b. a -> b -> Pair a b
Ambiguity check for forall a b. a -> b -> Pair a b
tcSubType_NC
  the type of the constructor ‘MkPair’
  forall a b. a -> b -> Pair a b
  forall a b. a -> b -> Pair a b
tc_sub_tc_type (general case)
  ty_actual   = forall a b. a -> b -> Pair a b
  ty_expected = forall a b. a -> b -> Pair a b
tcSkolemise
tcSkolemise
  0
  expected_ty forall a b. a -> b -> Pair a b
  inst tyvars [(a_a1bf, a_a1bG[sk:1]), (b_a1bg, b_a1bH[sk:1])]
  given []
  inst type a_a1bG[sk:1]
            -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
pushLevelAndCaptureConstraints { 1
tc_sub_type_ds
  ty_actual   = forall a b. a -> b -> Pair a b
  ty_expected = a_a1bG[sk:1]
                -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
cloneAnonMetaTyVar a_a1bI[tau:1]
cloneAnonMetaTyVar b_a1bJ[tau:1]
Instantiating
  all tyvars? True
  origin arising from a type equality forall a b. a -> b -> Pair a b
                                      ~
                                      forall a b. a -> b -> Pair a b
  type forall @a_a1bf @b_a1bg. a_a1bf -> b_a1bg -> Pair a_a1bf b_a1bg
  theta []
  leave_bndrs []
  with a_a1bI[tau:1]
       b_a1bJ[tau:1]
  theta: []
tc_sub_type_ds
  ty_actual   = a_a1bI[tau:1]
                -> b_a1bJ[tau:1] -> Pair a_a1bI[tau:1] b_a1bJ[tau:1]
  ty_expected = a_a1bG[sk:1]
                -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
tc_sub_type_ds
  ty_actual   = b_a1bJ[tau:1] -> Pair a_a1bI[tau:1] b_a1bJ[tau:1]
  ty_expected = b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
tc_sub_type_ds
  ty_actual   = Pair a_a1bI[tau:1] b_a1bJ[tau:1]
  ty_expected = Pair a_a1bG[sk:1] b_a1bH[sk:1]
deeply_instantiate final subst
  origin: arising from a type equality forall a b. a -> b -> Pair a b
                                       ~
                                       forall a b. a -> b -> Pair a b
  type: Pair a_a1bI[tau:1] b_a1bJ[tau:1]
  new type: Pair a_a1bI[tau:1] b_a1bJ[tau:1]
  subst: [TCvSubst
            In scope: InScope {a_a1bI b_a1bJ}
            Type env: []
            Co env: []]
u_tys
  tclvl 1
  Pair a_a1bI[tau:1] b_a1bJ[tau:1] ~ Pair a_a1bG[sk:1] b_a1bH[sk:1]
  arising from a type equality a_a1bI[tau:1]
                               -> b_a1bJ[tau:1] -> Pair a_a1bI[tau:1] b_a1bJ[tau:1]
                               ~
                               a_a1bG[sk:1] -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
u_tys
  tclvl 1
  a_a1bI[tau:1] ~ a_a1bG[sk:1]
  arising from a type equality a_a1bI[tau:1]
                               -> b_a1bJ[tau:1] -> Pair a_a1bI[tau:1] b_a1bJ[tau:1]
                               ~
                               a_a1bG[sk:1] -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    a_a1bI[tau:1] ~ a_a1bG[sk:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    a_a1bI[tau:1] ~ a_a1bG[sk:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  a_a1bI[tau:1] :: *
  a_a1bG[sk:1] :: *
  True
  <*>_N
writeMetaTyVar a_a1bI[tau:1] := a_a1bG[sk:1]
u_tys yields no coercion
u_tys
  tclvl 1
  b_a1bJ[tau:1] ~ b_a1bH[sk:1]
  arising from a type equality a_a1bI[tau:1]
                               -> b_a1bJ[tau:1] -> Pair a_a1bI[tau:1] b_a1bJ[tau:1]
                               ~
                               a_a1bG[sk:1] -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    b_a1bJ[tau:1] ~ b_a1bH[sk:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    b_a1bJ[tau:1] ~ b_a1bH[sk:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  b_a1bJ[tau:1] :: *
  b_a1bH[sk:1] :: *
  True
  <*>_N
writeMetaTyVar b_a1bJ[tau:1] := b_a1bH[sk:1]
u_tys yields no coercion
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = b_a1bH[sk:1]
  ty_expected = b_a1bJ[tau:1]
tcSkolemise
tcSkolemise
  1
  expected_ty b_a1bJ[tau:1]
  inst tyvars []
  given []
  inst type b_a1bJ[tau:1]
tc_sub_type_ds
  ty_actual   = b_a1bH[sk:1]
  ty_expected = b_a1bJ[tau:1]
u_tys
  tclvl 1
  b_a1bH[sk:1] ~ b_a1bJ[tau:1]
  arising from a type equality a_a1bI[tau:1]
                               -> b_a1bJ[tau:1] -> Pair a_a1bI[tau:1] b_a1bJ[tau:1]
                               ~
                               a_a1bG[sk:1] -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = a_a1bG[sk:1]
  ty_expected = a_a1bI[tau:1]
tcSkolemise
tcSkolemise
  1
  expected_ty a_a1bI[tau:1]
  inst tyvars []
  given []
  inst type a_a1bI[tau:1]
tc_sub_type_ds
  ty_actual   = a_a1bG[sk:1]
  ty_expected = a_a1bI[tau:1]
u_tys
  tclvl 1
  a_a1bG[sk:1] ~ a_a1bI[tau:1]
  arising from a type equality a_a1bI[tau:1]
                               -> b_a1bJ[tau:1] -> Pair a_a1bI[tau:1] b_a1bJ[tau:1]
                               ~
                               a_a1bG[sk:1] -> b_a1bH[sk:1] -> Pair a_a1bG[sk:1] b_a1bH[sk:1]
u_tys yields no coercion
pushLevelAndCaptureConstraints } 1
checkConstraints
  1
  [a_a1bG[sk:1], b_a1bH[sk:1]]
simplifyAmbiguityCheck {
  type =  forall a b. a -> b -> Pair a b
  wanted =  WC {}
newTcEvBinds unique = a1bK
solveWanteds {
  Level = 0
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End simplifyAmbiguityCheck }
reportUnsolved(ambig) {
newTcEvBinds unique = a1bL
reportUnsolved(ambig) }
Done ambiguity check for forall a b. a -> b -> Pair a b
checkValidType done forall a b. a -> b -> Pair a b :: *
Done validity of data con
  MkPair
  Datacon user type: forall a b. a -> b -> Pair a b
  Datacon rep type: forall a b. a -> b -> Pair a b
  Rep typcon binders: [anon-vis (a_a1bf), anon-vis (b_a1bg)]
  not family
Done validity for tycon Pair
Done validity check [Pair]
---- end tcTyClGroup ---- }
tcAddTyCons
  tycons [Pair]
  implicits [Data constructor ‘MkPair’, Identifier ‘Foo.MkPair’]
tcExtendKindEnvList []
tc_extend_local_env []
Adding instances:
addFamInsts
tcAddTyCons
  tycons []
  implicits []
tcExtendKindEnvList []
tc_extend_local_env []
---- tcTyClGroup ---- {
Decls for [ShowPairlike]
tcExtendKindEnv [rsa :-> APromotionErr ClassPE]
---- kcTyClGroup ---- {
  module Foo
  class (forall c_a1be.
         c_a1be ~ Pair a_a1bc =>
         Show (c_a1be b_a1bd)) => ShowPairlike a_a1bc b_a1bd
getInitialKinds {
getInitialKinds done }
tcExtendKindEnvList []
solveEqualities { level = 1
getInitialKinds {
bindImplicitTKBndrs
  []
  []
tc_extend_local_env []
tcExtendBinderStack []
bindExplicTKBndrs [a_a1bc, b_a1bd]
newMetaKindVar k_a1bM[tau:1]
newTyVarTyVar a_a1bN[tyv:1]
tc_extend_local_env
  [(a_a1bc, Type variable ‘a_a1bc’ = a_a1bN[tyv:1] :: k_a1bM[tau:1])]
tcExtendBinderStack [a_a1bc a_a1bN[tyv:1]]
newMetaKindVar k_a1bO[tau:1]
newTyVarTyVar b_a1bP[tyv:1]
tc_extend_local_env
  [(b_a1bd, Type variable ‘b_a1bd’ = b_a1bP[tyv:1] :: k_a1bO[tau:1])]
tcExtendBinderStack [b_a1bd b_a1bP[tyv:1]]
kcLHsQTyVars: not-cusk
  ShowPairlike
  []
  [a_a1bc, b_a1bd]
  []
  [a_a1bN[tyv:1], b_a1bP[tyv:1]]
  k_a1bM[tau:1] -> k_a1bO[tau:1] -> Constraint
tc_extend_local_env
  [(a_a1bc, Type variable ‘a_a1bc’ = a_a1bN[tyv:1] :: k_a1bM[tau:1]),
   (b_a1bd, Type variable ‘b_a1bd’ = b_a1bP[tyv:1] :: k_a1bO[tau:1])]
tcExtendBinderStack [a_a1bc a_a1bN[tyv:1], b_a1bd b_a1bP[tyv:1]]
getInitialKinds done }
kcTyClGroup: initial kinds
  ShowPairlike :: k_a1bM[tau:1] -> k_a1bO[tau:1] -> Constraint
tcExtendKindEnvList
  [(ShowPairlike,
    ATcTyCon ShowPairlike[tc] :: k_a1bM[tau:1]
                                 -> k_a1bO[tau:1] -> Constraint)]
kcTyClDecl { ShowPairlike
bindTyClTyVars
  ShowPairlike [anon-vis (a_a1bN[tyv:1]), anon-vis (b_a1bP[tyv:1])]
  [(a_a1bc, a_a1bN[tyv:1]), (b_a1bd, b_a1bP[tyv:1])]
tc_extend_local_env
  [(a_a1bc, Type variable ‘a_a1bc’ = a_a1bN[tyv:1] :: k_a1bM[tau:1]),
   (b_a1bd, Type variable ‘b_a1bd’ = b_a1bP[tyv:1] :: k_a1bO[tau:1])]
tcExtendBinderStack [a_a1bc a_a1bN[tyv:1], b_a1bd b_a1bP[tyv:1]]
pushLevelAndCaptureConstraints { 2
bindExplicTKBndrs [c_a1be]
newMetaKindVar k_a1bQ[tau:2]
tc_extend_local_env
  [(c_a1be, Type variable ‘c_a1be’ = c_a1be[sk:2] :: k_a1bQ[tau:2])]
tcExtendBinderStack [c_a1be c_a1be[sk:2]]
lk1 ~
tcInferApps {
  (~)
  [c_a1be, Pair a_a1bc]
tcInferApps (need to instantiate)
  @k_10
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
cloneAnonMetaTyVar k_a1bR[tau:2]
tcInferApps (vis normal app)
  [vis] k_10
  c_a1be
  k_10
  [TCvSubst
     In scope: InScope {k_a1bR}
     Type env: [10 :-> k_a1bR[tau:2]]
     Co env: []]
lk1 c_a1be
tcInferApps {
  c_a1be
  []
tcInferApps } c_a1be[sk:2] :: k_a1bQ[tau:2]
checkExpectedKind
  c_a1be[sk:2]
  k_a1bQ[tau:2]
checkExpectedKindX
  c_a1be
  act_kind': k_a1bQ[tau:2]
  exp_kind: k_a1bR[tau:2]
u_tys
  tclvl 2
  k_a1bQ[tau:2] ~ k_a1bR[tau:2]
  arising from a type equality k_a1bQ[tau:2] ~ k_a1bR[tau:2]
u_tys
  tclvl 2
  * ~ *
  arising from a kind equality arising from
    k_a1bQ[tau:2] ~ k_a1bR[tau:2]
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    k_a1bQ[tau:2] ~ k_a1bR[tau:2]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1bQ[tau:2] :: *
  k_a1bR[tau:2] :: *
  True
  <*>_N
writeMetaTyVar k_a1bQ[tau:2] := k_a1bR[tau:2]
u_tys yields no coercion
checkExpectedKind
  k_a1bQ[tau:2]
  k_a1bR[tau:2]
  <k_a1bR[tau:2]>_N
tcInferApps (vis normal app) 2 k_a1bR[tau:2]
tcInferApps (vis normal app)
  [vis] k_10
  Pair a_a1bc
  k_10
  [TCvSubst
     In scope: InScope {k_a1bR}
     Type env: [10 :-> k_a1bR[tau:2]]
     Co env: []]
lk1 Pair
tcInferApps {
  Pair
  [a_a1bc]
tcInferApps (vis normal app)
  [vis] *
  a_a1bc
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1bc
tcInferApps {
  a_a1bc
  []
tcInferApps } a_a1bN[tyv:1] :: k_a1bM[tau:1]
checkExpectedKind
  a_a1bN[tyv:1]
  k_a1bM[tau:1]
checkExpectedKindX
  a_a1bc
  act_kind': k_a1bM[tau:1]
  exp_kind: *
u_tys
  tclvl 2
  k_a1bM[tau:1] ~ *
  arising from a type equality k_a1bM[tau:1] ~ *
uUnfilledVar2 not ok
  k_a1bM[tau:1]
  *
New coercion hole: co_a1bS
utype_defer
  k_a1bM[tau:1]
  TYPE 'GHC.Types.LiftedRep
  arising from a type equality k_a1bM[tau:1] ~ *
  In the first argument of ‘Pair’, namely ‘a_a1bc’
  In the second argument of ‘(~)’, namely ‘Pair a_a1bc’
  In the class declaration for ‘ShowPairlike’
utype_defer2 {co_a1bS}
u_tys yields coercion: {co_a1bS}
checkExpectedKind
  k_a1bM[tau:1]
  *
  {co_a1bS}
tcInferApps (vis normal app) 2 *
tcInferApps } Pair (a_a1bN[tyv:1] |> {co_a1bS}) :: * -> *
checkExpectedKind
  Pair (a_a1bN[tyv:1] |> {co_a1bS})
  * -> *
checkExpectedKindX
  Pair a_a1bc
  act_kind': * -> *
  exp_kind: k_a1bR[tau:2]
u_tys
  tclvl 2
  * -> * ~ k_a1bR[tau:2]
  arising from a type equality * -> * ~ k_a1bR[tau:2]
u_tys
  tclvl 2
  * ~ *
  arising from a kind equality arising from k_a1bR[tau:2] ~ * -> *
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1bR[tau:2] ~ * -> *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1bR[tau:2] :: *
  * -> * :: *
  True
  <*>_N
writeMetaTyVar k_a1bR[tau:2] := * -> *
u_tys yields no coercion
checkExpectedKind
  * -> *
  k_a1bR[tau:2]
  <* -> *>_N
tcInferApps (vis normal app) 2 k_a1bR[tau:2]
tcInferApps }
  c_a1be[sk:2] ~ Pair (a_a1bN[tyv:1] |> {co_a1bS}) :: Constraint
checkExpectedKind
  c_a1be[sk:2] ~ Pair (a_a1bN[tyv:1] |> {co_a1bS})
  Constraint
checkExpectedKindX
  c_a1be ~ Pair a_a1bc
  act_kind': Constraint
  exp_kind: Constraint
lk1 Show
tcInferApps {
  Show
  [(c_a1be b_a1bd)]
tcInferApps (vis normal app)
  [vis] *
  (c_a1be b_a1bd)
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 c_a1be
tcInferApps {
  c_a1be
  [b_a1bd]
tcInferApps (no binder)
  c_a1be[sk:2] :: k_a1bQ[tau:2]
  1
  <* -> *>_N
  c_a1be[sk:2] :: * -> *
tcInferApps (vis normal app)
  [vis] *
  b_a1bd
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bd
tcInferApps {
  b_a1bd
  []
tcInferApps } b_a1bP[tyv:1] :: k_a1bO[tau:1]
checkExpectedKind
  b_a1bP[tyv:1]
  k_a1bO[tau:1]
checkExpectedKindX
  b_a1bd
  act_kind': k_a1bO[tau:1]
  exp_kind: *
u_tys
  tclvl 2
  k_a1bO[tau:1] ~ *
  arising from a type equality k_a1bO[tau:1] ~ *
uUnfilledVar2 not ok
  k_a1bO[tau:1]
  *
New coercion hole: co_a1bZ
utype_defer
  k_a1bO[tau:1]
  TYPE 'GHC.Types.LiftedRep
  arising from a type equality k_a1bO[tau:1] ~ *
  In the first argument of ‘c_a1be’, namely ‘b_a1bd’
  In the first argument of ‘Show’, namely ‘(c_a1be b_a1bd)’
  In the class declaration for ‘ShowPairlike’
utype_defer2 {co_a1bZ}
u_tys yields coercion: {co_a1bZ}
checkExpectedKind
  k_a1bO[tau:1]
  *
  {co_a1bZ}
tcInferApps (vis normal app) 2 *
tcInferApps } c_a1be[sk:2] (b_a1bP[tyv:1] |> {co_a1bZ}) :: *
checkExpectedKind
  c_a1be[sk:2] (b_a1bP[tyv:1] |> {co_a1bZ})
  *
checkExpectedKindX
  c_a1be b_a1bd
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps }
  Show (c_a1be[sk:2] (b_a1bP[tyv:1] |> {co_a1bZ})) :: Constraint
checkExpectedKind
  Show (c_a1be[sk:2] (b_a1bP[tyv:1] |> {co_a1bZ}))
  Constraint
checkExpectedKindX
  Show (c_a1be b_a1bd)
  act_kind': Constraint
  exp_kind: Constraint
pushLevelAndCaptureConstraints } 2
newNoTcEvBinds unique = a1c0
kcTyClDecl done } ShowPairlike
solveEqualities: running solver
  wanted =  WC {wc_impl =
                  Implic {
                    TcLevel = 2
                    Skolems = (c_a1be[sk:2] :: k_a1bQ[tau:2])
                    No-eqs = True
                    Status = Unsolved
                    Given =
                    Wanted =
                      WC {wc_simple =
                            [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                                     GHC.Prim.~# * (CNonCanonical)
                            [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                                     GHC.Prim.~# * (CNonCanonical)}
                    Binds = CoEvBindsVar<a1c0>
                    ‘forall c_a1be. c_a1be ~ Pair a_a1bc => Show (c_a1be b_a1bd)’ }}
newNoTcEvBinds unique = a1c1
solveWanteds {
  Level = 1
  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems = (c_a1be[sk:2] :: k_a1bQ[tau:2])
          No-eqs = True
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                           GHC.Prim.~# * (CNonCanonical)
                  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                           GHC.Prim.~# * (CNonCanonical)}
          Binds = CoEvBindsVar<a1c0>
          ‘forall c_a1be. c_a1be ~ Pair a_a1bc => Show (c_a1be b_a1bd)’ }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 2
    Skolems = (c_a1be[sk:2] :: k_a1bQ[tau:2])
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                     GHC.Prim.~# * (CNonCanonical)
            [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                     GHC.Prim.~# * (CNonCanonical)}
    Binds = CoEvBindsVar<a1c0>
    ‘forall c_a1be. c_a1be ~ Pair a_a1bc => Show (c_a1be b_a1bd)’ }
  Inerts {Unsolved goals = 0}
         Inert fsks = []
solveWanteds {
  Level = 2
  WC {wc_simple =
        [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                 GHC.Prim.~# * (CNonCanonical)
        [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                 GHC.Prim.~# * (CNonCanonical)}
solveSimpleWanteds {
  {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                            GHC.Prim.~# * (CNonCanonical),
   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                       GHC.Prim.~# * (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {Eqs = [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                                        GHC.Prim.~# * (CNonCanonical)}
runStage canonicalization {
  workitem   =  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                         GHC.Prim.~# * (CNonCanonical)
canEvNC:eq
  k_a1bM[tau:1]
  *
can_eq_nc
  False
  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1] GHC.Prim.~# *
  nominal equality
  k_a1bM[tau:1]
  k_a1bM[tau:1]
  *
  *
flatten { FM_FlattenAll k_a1bM[tau:1]
Unfilled tyvar k_a1bM[tau:1]
flatten } k_a1bM[tau:1]
flatten { FM_FlattenAll *
flatten } *
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
doTopReact
  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1] GHC.Prim.~# * (CTyEqCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                                            GHC.Prim.~# * (CTyEqCan)
addInertCan }
Step 1[l:2,d:0] Kept as inert:
    [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1] GHC.Prim.~# *
End solver pipeline (kept as inert) }
  final_item = [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                        GHC.Prim.~# * (CTyEqCan)
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                       GHC.Prim.~# * (CNonCanonical)
  inerts = {Equalities: [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                                 GHC.Prim.~# * (CTyEqCan)
            Unsolved goals = 1}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                         GHC.Prim.~# * (CNonCanonical)
canEvNC:eq
  k_a1bO[tau:1]
  *
can_eq_nc
  False
  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# *
  nominal equality
  k_a1bO[tau:1]
  k_a1bO[tau:1]
  *
  *
flatten { FM_FlattenAll k_a1bO[tau:1]
Unfilled tyvar k_a1bO[tau:1]
flatten } k_a1bO[tau:1]
flatten { FM_FlattenAll *
flatten } *
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
doTopReact
  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# * (CTyEqCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                                            GHC.Prim.~# * (CTyEqCan)
addInertCan }
Step 2[l:2,d:0] Kept as inert:
    [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# *
End solver pipeline (kept as inert) }
  final_item = [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                        GHC.Prim.~# * (CTyEqCan)
getUnsolvedInerts
   tv eqs = {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                      GHC.Prim.~# * (CTyEqCan),
             [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# * (CTyEqCan)}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs = [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                     GHC.Prim.~# * (CTyEqCan)
            [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# * (CTyEqCan)}
Unflattening 1 {}
Unflattening 2
  {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1] GHC.Prim.~# * (CTyEqCan)
   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# * (CTyEqCan)}
Unflattening 3 {}
Unflattening done
  {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                            GHC.Prim.~# * (CNonCanonical)
   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {wc_simple =
                   [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)
                   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)}
solveWanteds }
  final wc = WC {wc_simple =
                   [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)
                   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)}
  current evbinds  = {}
getNoGivenEqs
  No given equalities
  Skols: [c_a1be[sk:2]]
  Inerts: {Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done:
  {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                            GHC.Prim.~# * (CNonCanonical),
   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
floatEqualities
  Skols = [c_a1be[sk:2]]
  Extended skols = {c_a1be[sk:2]}
  Simples = {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                      GHC.Prim.~# * (CNonCanonical),
             [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                      GHC.Prim.~# * (CNonCanonical)}
  Candidate eqs = {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                            GHC.Prim.~# * (CNonCanonical),
                   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)}
  Floated eqs = {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                          GHC.Prim.~# * (CNonCanonical),
                 [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                          GHC.Prim.~# * (CNonCanonical)}
solveImplication 2
  {}
  WC {}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 2
    Skolems = (c_a1be[sk:2] :: k_a1bQ[tau:2])
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted = WC {}
    Binds = CoEvBindsVar<a1c0>
    ‘forall c_a1be. c_a1be ~ Pair a_a1bc => Show (c_a1be b_a1bd)’ }
neededEvVars
  old_needs: {}
  seeds3: {}
  tcvs: {}
  ev_binds: []
  live_ev_binds: []
setImplicationStatus(all-solved) }
  discard: True
  new_implic: Implic {
                TcLevel = 2
                Skolems = (c_a1be[sk:2] :: k_a1bQ[tau:2])
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given =
                Wanted = WC {}
                Binds = CoEvBindsVar<a1c0>
                ‘forall c_a1be. c_a1be ~ Pair a_a1bc => Show (c_a1be b_a1bd)’ }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                          GHC.Prim.~# * (CNonCanonical),
                 [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                          GHC.Prim.~# * (CNonCanonical)}
  res_implic = Nothing
  implication evbinds = {}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                              GHC.Prim.~# * (CNonCanonical),
                     [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                              GHC.Prim.~# * (CNonCanonical)}
  unsolved_implics = {Nothing}
simpl_loop iteration=0 (no new given superclasses = True, 2 simples to solve)
simpl_loop: wc =
  WC {wc_simple =
        [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                 GHC.Prim.~# * (CNonCanonical)
        [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                 GHC.Prim.~# * (CNonCanonical)}
solveSimpleWanteds {
  {[WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                            GHC.Prim.~# * (CNonCanonical),
   [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                       GHC.Prim.~# * (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {Eqs = [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                                        GHC.Prim.~# * (CNonCanonical)}
runStage canonicalization {
  workitem   =  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                         GHC.Prim.~# * (CNonCanonical)
canEvNC:eq
  k_a1bM[tau:1]
  *
can_eq_nc
  False
  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1] GHC.Prim.~# *
  nominal equality
  k_a1bM[tau:1]
  k_a1bM[tau:1]
  *
  *
flatten { FM_FlattenAll k_a1bM[tau:1]
Unfilled tyvar k_a1bM[tau:1]
flatten } k_a1bM[tau:1]
flatten { FM_FlattenAll *
flatten } *
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
Sneaky unification:
  Unifies: k_a1bM[tau:1] := *
  Coercion: k_a1bM[tau:1] ~ *
  Left Kind is: *
  Right Kind is: *
unifyTyVar k_a1bM[tau:1] := *
writeMetaTyVar k_a1bM[tau:1] := *
Filling coercion hole co_a1bS := <*>_N
end stage interact with inerts }
Step 3[l:1,d:0] Solved by unification:
    [WD] hole{co_a1bS} {0}:: k_a1bM[tau:1] GHC.Prim.~# *
End solver pipeline (discharged) }
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                       GHC.Prim.~# * (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                         GHC.Prim.~# * (CNonCanonical)
canEvNC:eq
  k_a1bO[tau:1]
  *
can_eq_nc
  False
  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# *
  nominal equality
  k_a1bO[tau:1]
  k_a1bO[tau:1]
  *
  *
flatten { FM_FlattenAll k_a1bO[tau:1]
Unfilled tyvar k_a1bO[tau:1]
flatten } k_a1bO[tau:1]
flatten { FM_FlattenAll *
flatten } *
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
Sneaky unification:
  Unifies: k_a1bO[tau:1] := *
  Coercion: k_a1bO[tau:1] ~ *
  Left Kind is: *
  Right Kind is: *
unifyTyVar k_a1bO[tau:1] := *
writeMetaTyVar k_a1bO[tau:1] := *
Filling coercion hole co_a1bZ := <*>_N
end stage interact with inerts }
Step 4[l:1,d:0] Solved by unification:
    [WD] hole{co_a1bZ} {0}:: k_a1bO[tau:1] GHC.Prim.~# *
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
Constraint solver steps = 4
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1c2
Skolemising a_a1bN[tyv:1] := a_a1bN[sk:1]
writeMetaTyVar a_a1bN[tyv:1] := a_a1bN[sk:1]
Skolemising b_a1bP[tyv:1] := b_a1bP[sk:1]
writeMetaTyVar b_a1bP[tyv:1] := b_a1bP[sk:1]
quantifyTyVars 1
  0
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
  {}
quantifyTyVars 2
  globals: {}
  mono_tvs: {}
  nondep:
  dep:
  dep_kvs'
  nondep_tvs'
generaliseTcTyCon: before zonkRec
  spec_req_tvs = a_a1bN[sk:1] b_a1bP[sk:1]
  inferred =
generaliseTcTyCon: post zonk
  tycon = ShowPairlike[tc]
  inferred =
  ze = ZE {ze_tv_env = [a1bN :-> a_a1bc, a1bP :-> b_a1bd]
           ze_id_env = []}
  spec_req_prs = [(a_a1bc, a_a1bN[tyv:1]), (b_a1bd, b_a1bP[tyv:1])]
  spec_req_tvs = a_a1bN[sk:1] b_a1bP[sk:1]
  final_spec_req_tvs = a_a1bc b_a1bd
generaliseTcTyCon done
  tycon = ShowPairlike[tc]
  tc_res_kind = Constraint
  dep_fv_set = {}
  final_spec_req_tvs = a_a1bc b_a1bd
  inferred =
  specified =
  required_tcbs = [anon-vis (a_a1bc), anon-vis (b_a1bd)]
  final_tcbs = [anon-vis (a_a1bc), anon-vis (b_a1bd)]
---- kcTyClGroup end ---- } ShowPairlike :: * -> * -> Constraint
tcTyAndCl generalized kinds
  (ShowPairlike,
   [anon-vis (a_a1bc), anon-vis (b_a1bd)],
   Constraint
   True)
tcExtendKindEnvList
  [(ShowPairlike, ATcTyCon ShowPairlike[tc] :: * -> * -> Constraint)]
---- tcTyClDecl ---- {
  class (forall c_a1be.
         c_a1be ~ Pair a_a1bc =>
         Show (c_a1be b_a1bd)) => ShowPairlike a_a1bc b_a1bd
bindTyClTyVars
  ShowPairlike [anon-vis (a_a1bc), anon-vis (b_a1bd)]
  [(a_a1bc, a_a1bc), (b_a1bd, b_a1bd)]
tc_extend_local_env
  [(a_a1bc, Type variable ‘a_a1bc’ = a_a1bc :: *),
   (b_a1bd, Type variable ‘b_a1bd’ = b_a1bd :: *)]
tcExtendBinderStack [a_a1bc a_a1bc, b_a1bd b_a1bd]
tcClassDecl 1
  ShowPairlike
  [anon-vis (a_a1bc), anon-vis (b_a1bd)]
solveEqualities { level = 1
pushLevelAndCaptureConstraints { 2
bindExplicTKBndrs [c_a1be]
newMetaKindVar k_a1c3[tau:2]
tc_extend_local_env
  [(c_a1be, Type variable ‘c_a1be’ = c_a1be[sk:2] :: k_a1c3[tau:2])]
tcExtendBinderStack [c_a1be c_a1be[sk:2]]
lk1 ~
tcInferApps {
  (~)
  [c_a1be, Pair a_a1bc]
tcInferApps (need to instantiate)
  @k_10
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
cloneAnonMetaTyVar k_a1c4[tau:2]
tcInferApps (vis normal app)
  [vis] k_10
  c_a1be
  k_10
  [TCvSubst
     In scope: InScope {k_a1c4}
     Type env: [10 :-> k_a1c4[tau:2]]
     Co env: []]
lk1 c_a1be
tcInferApps {
  c_a1be
  []
tcInferApps } c_a1be[sk:2] :: k_a1c3[tau:2]
checkExpectedKind
  c_a1be[sk:2]
  k_a1c3[tau:2]
checkExpectedKindX
  c_a1be
  act_kind': k_a1c3[tau:2]
  exp_kind: k_a1c4[tau:2]
u_tys
  tclvl 2
  k_a1c3[tau:2] ~ k_a1c4[tau:2]
  arising from a type equality k_a1c3[tau:2] ~ k_a1c4[tau:2]
u_tys
  tclvl 2
  * ~ *
  arising from a kind equality arising from
    k_a1c3[tau:2] ~ k_a1c4[tau:2]
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    k_a1c3[tau:2] ~ k_a1c4[tau:2]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1c3[tau:2] :: *
  k_a1c4[tau:2] :: *
  True
  <*>_N
writeMetaTyVar k_a1c3[tau:2] := k_a1c4[tau:2]
u_tys yields no coercion
checkExpectedKind
  k_a1c3[tau:2]
  k_a1c4[tau:2]
  <k_a1c4[tau:2]>_N
tcInferApps (vis normal app) 2 k_a1c4[tau:2]
tcInferApps (vis normal app)
  [vis] k_10
  Pair a_a1bc
  k_10
  [TCvSubst
     In scope: InScope {k_a1c4}
     Type env: [10 :-> k_a1c4[tau:2]]
     Co env: []]
lk1 Pair
tcInferApps {
  Pair
  [a_a1bc]
tcInferApps (vis normal app)
  [vis] *
  a_a1bc
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1bc
tcInferApps {
  a_a1bc
  []
tcInferApps } a_a1bc :: *
checkExpectedKind
  a_a1bc
  *
checkExpectedKindX
  a_a1bc
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps } Pair a_a1bc :: * -> *
checkExpectedKind
  Pair a_a1bc
  * -> *
checkExpectedKindX
  Pair a_a1bc
  act_kind': * -> *
  exp_kind: k_a1c4[tau:2]
u_tys
  tclvl 2
  * -> * ~ k_a1c4[tau:2]
  arising from a type equality * -> * ~ k_a1c4[tau:2]
u_tys
  tclvl 2
  * ~ *
  arising from a kind equality arising from k_a1c4[tau:2] ~ * -> *
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1c4[tau:2] ~ * -> *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1c4[tau:2] :: *
  * -> * :: *
  True
  <*>_N
writeMetaTyVar k_a1c4[tau:2] := * -> *
u_tys yields no coercion
checkExpectedKind
  * -> *
  k_a1c4[tau:2]
  <* -> *>_N
tcInferApps (vis normal app) 2 k_a1c4[tau:2]
tcInferApps } c_a1be[sk:2] ~ Pair a_a1bc :: Constraint
checkExpectedKind
  c_a1be[sk:2] ~ Pair a_a1bc
  Constraint
checkExpectedKindX
  c_a1be ~ Pair a_a1bc
  act_kind': Constraint
  exp_kind: Constraint
lk1 Show
tcInferApps {
  Show
  [(c_a1be b_a1bd)]
tcInferApps (vis normal app)
  [vis] *
  (c_a1be b_a1bd)
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 c_a1be
tcInferApps {
  c_a1be
  [b_a1bd]
tcInferApps (no binder)
  c_a1be[sk:2] :: k_a1c3[tau:2]
  1
  <* -> *>_N
  c_a1be[sk:2] :: * -> *
tcInferApps (vis normal app)
  [vis] *
  b_a1bd
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bd
tcInferApps {
  b_a1bd
  []
tcInferApps } b_a1bd :: *
checkExpectedKind
  b_a1bd
  *
checkExpectedKindX
  b_a1bd
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps } c_a1be[sk:2] b_a1bd :: *
checkExpectedKind
  c_a1be[sk:2] b_a1bd
  *
checkExpectedKindX
  c_a1be b_a1bd
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps } Show (c_a1be[sk:2] b_a1bd) :: Constraint
checkExpectedKind
  Show (c_a1be[sk:2] b_a1bd)
  Constraint
checkExpectedKindX
  Show (c_a1be b_a1bd)
  act_kind': Constraint
  exp_kind: Constraint
pushLevelAndCaptureConstraints } 2
tcClassSigs 1 ShowPairlike
tcClassSigs 2 ShowPairlike
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1c5
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1c6
tcClassDecl
  []
  [anon-vis (a_a1bc), anon-vis (b_a1bd)]
  []
---- tcTyClDecl end ---- } ShowPairlike
Starting synonym cycle check [ShowPairlike]
Done synonym cycle check [ShowPairlike]
Starting validity check [ShowPairlike]
Starting validity for tycon ShowPairlike
checkValidTyCon
  ShowPairlike
  Just ShowPairlike
check_valid_theta
  [forall (c :: * -> *). (c ~ Pair a_a1bc) => Show (c b_a1bd)]
check_type
  forall (c :: * -> *). (c ~ Pair a_a1bc) => Show (c b_a1bd)
  True
check_valid_theta [c_a1be ~ Pair a_a1bc]
Done validity for tycon ShowPairlike
Done validity check [ShowPairlike]
---- end tcTyClGroup ---- }
tcAddTyCons
  tycons [ShowPairlike]
  implicits [Identifier ‘Foo.$p1ShowPairlike’,
             Coercion axiom ‘Foo.N:ShowPairlike’,
             Data constructor ‘Foo.C:ShowPairlike’,
             Identifier ‘Foo.C:ShowPairlike’]
tcExtendKindEnvList []
tc_extend_local_env []
tcLocalInstDecl
  Show (Pair a_a1bj b_a1bk) => ShowPairlike a_a1bj b_a1bk
tcTopLHsType {
  Show (Pair a_a1bj b_a1bk) => ShowPairlike a_a1bj b_a1bk
solveEqualities { level = 1
newMetaKindVar k_a1cf[tau:1]
newMetaKindVar k_a1cg[tau:1]
bindImplicitTKBndrs
  [a_a1bj, b_a1bk]
  [a_a1bj[sk:1], b_a1bk[sk:1]]
tc_extend_local_env
  [(a_a1bj, Type variable ‘a_a1bj’ = a_a1bj[sk:1] :: k_a1cf[tau:1]),
   (b_a1bk, Type variable ‘b_a1bk’ = b_a1bk[sk:1] :: k_a1cg[tau:1])]
tcExtendBinderStack [a_a1bj a_a1bj[sk:1], b_a1bk b_a1bk[sk:1]]
lk1 Show
tcInferApps {
  Show
  [(Pair a_a1bj b_a1bk)]
tcInferApps (vis normal app)
  [vis] *
  (Pair a_a1bj b_a1bk)
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 Pair
tcInferApps {
  Pair
  [a_a1bj, b_a1bk]
tcInferApps (vis normal app)
  [vis] *
  a_a1bj
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1bj
tcInferApps {
  a_a1bj
  []
tcInferApps } a_a1bj[sk:1] :: k_a1cf[tau:1]
checkExpectedKind
  a_a1bj[sk:1]
  k_a1cf[tau:1]
checkExpectedKindX
  a_a1bj
  act_kind': k_a1cf[tau:1]
  exp_kind: *
u_tys
  tclvl 1
  k_a1cf[tau:1] ~ *
  arising from a type equality k_a1cf[tau:1] ~ *
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from k_a1cf[tau:1] ~ *
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1cf[tau:1] ~ *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1cf[tau:1] :: *
  * :: *
  True
  <*>_N
writeMetaTyVar k_a1cf[tau:1] := *
u_tys yields no coercion
checkExpectedKind
  k_a1cf[tau:1]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps (vis normal app)
  [vis] *
  b_a1bk
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bk
tcInferApps {
  b_a1bk
  []
tcInferApps } b_a1bk[sk:1] :: k_a1cg[tau:1]
checkExpectedKind
  b_a1bk[sk:1]
  k_a1cg[tau:1]
checkExpectedKindX
  b_a1bk
  act_kind': k_a1cg[tau:1]
  exp_kind: *
u_tys
  tclvl 1
  k_a1cg[tau:1] ~ *
  arising from a type equality k_a1cg[tau:1] ~ *
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from k_a1cg[tau:1] ~ *
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1cg[tau:1] ~ *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1cg[tau:1] :: *
  * :: *
  True
  <*>_N
writeMetaTyVar k_a1cg[tau:1] := *
u_tys yields no coercion
checkExpectedKind
  k_a1cg[tau:1]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps } Pair a_a1bj[sk:1] b_a1bk[sk:1] :: *
checkExpectedKind
  Pair a_a1bj[sk:1] b_a1bk[sk:1]
  *
checkExpectedKindX
  Pair a_a1bj b_a1bk
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps } Show (Pair a_a1bj[sk:1] b_a1bk[sk:1]) :: Constraint
checkExpectedKind
  Show (Pair a_a1bj[sk:1] b_a1bk[sk:1])
  Constraint
checkExpectedKindX
  Show (Pair a_a1bj b_a1bk)
  act_kind': Constraint
  exp_kind: Constraint
lk1 ShowPairlike
tcInferApps {
  ShowPairlike
  [a_a1bj, b_a1bk]
tcInferApps (vis normal app)
  [vis] *
  a_a1bj
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1bj
tcInferApps {
  a_a1bj
  []
tcInferApps } a_a1bj[sk:1] :: k_a1cf[tau:1]
checkExpectedKind
  a_a1bj[sk:1]
  k_a1cf[tau:1]
checkExpectedKindX
  a_a1bj
  act_kind': k_a1cf[tau:1]
  exp_kind: *
u_tys
  tclvl 1
  k_a1cf[tau:1] ~ *
  arising from a type equality k_a1cf[tau:1] ~ *
found filled tyvar k_a1cf[tau:1] :-> *
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a type equality k_a1cf[tau:1] ~ *
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  k_a1cf[tau:1]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps (vis normal app)
  [vis] *
  b_a1bk
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bk
tcInferApps {
  b_a1bk
  []
tcInferApps } b_a1bk[sk:1] :: k_a1cg[tau:1]
checkExpectedKind
  b_a1bk[sk:1]
  k_a1cg[tau:1]
checkExpectedKindX
  b_a1bk
  act_kind': k_a1cg[tau:1]
  exp_kind: *
u_tys
  tclvl 1
  k_a1cg[tau:1] ~ *
  arising from a type equality k_a1cg[tau:1] ~ *
found filled tyvar k_a1cg[tau:1] :-> *
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a type equality k_a1cg[tau:1] ~ *
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  k_a1cg[tau:1]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps } ShowPairlike a_a1bj[sk:1] b_a1bk[sk:1] :: Constraint
checkExpectedKind
  ShowPairlike a_a1bj[sk:1] b_a1bk[sk:1]
  Constraint
checkExpectedKindX
  ShowPairlike a_a1bj b_a1bk
  act_kind': Constraint
  exp_kind: Constraint
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1ch
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1ci
kindGeneralise1 forall a b. Show (Pair a b) => ShowPairlike a b
kindGeneralize
  forall a b. Show (Pair a b) => ShowPairlike a b
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
quantifyTyVars 1
  0
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
  {}
quantifyTyVars 2
  globals: {}
  mono_tvs: {}
  nondep:
  dep:
  dep_kvs'
  nondep_tvs'
End tcTopLHsType }
  Show (Pair a_a1bj b_a1bk) => ShowPairlike a_a1bj b_a1bk
  forall a b. Show (Pair a b) => ShowPairlike a b
checkValidInstance {
  forall a b. Show (Pair a b) => ShowPairlike a b
check_valid_theta [Show (Pair a_a1bj b_a1bk)]
Ambiguity check for forall a b. Show (Pair a b) => ShowPairlike a b
tcSubType_NC
  an instance declaration
  forall a b. Show (Pair a b) => ShowPairlike a b
  forall a b. Show (Pair a b) => ShowPairlike a b
tc_sub_tc_type (general case)
  ty_actual   = forall a b. Show (Pair a b) => ShowPairlike a b
  ty_expected = forall a b. Show (Pair a b) => ShowPairlike a b
tcSkolemise
tcSkolemise
  0
  expected_ty forall a b. Show (Pair a b) => ShowPairlike a b
  inst tyvars [(a_a1bj, a_a1ck[sk:1]), (b_a1bk, b_a1cl[sk:1])]
  given [$dShow_a1cm]
  inst type ShowPairlike a_a1ck[sk:1] b_a1cl[sk:1]
pushLevelAndCaptureConstraints { 1
tc_sub_type_ds
  ty_actual   = forall a b. Show (Pair a b) => ShowPairlike a b
  ty_expected = ShowPairlike a_a1ck[sk:1] b_a1cl[sk:1]
cloneAnonMetaTyVar a_a1cn[tau:1]
cloneAnonMetaTyVar b_a1co[tau:1]
instCallConstraints [$dShow_a1cp]
Instantiating
  all tyvars? True
  origin arising from a type equality forall a b.
                                      Show (Pair a b) =>
                                      ShowPairlike a b
                                      ~
                                      forall a b. Show (Pair a b) => ShowPairlike a b
  type forall @a_a1bj @b_a1bk.
         Show (Pair a_a1bj b_a1bk) => ShowPairlike a_a1bj b_a1bk
  theta [Show (Pair a_a1bj b_a1bk)]
  leave_bndrs []
  with a_a1cn[tau:1]
       b_a1co[tau:1]
  theta: [Show (Pair a_a1cn[tau:1] b_a1co[tau:1])]
tc_sub_type_ds
  ty_actual   = ShowPairlike a_a1cn[tau:1] b_a1co[tau:1]
  ty_expected = ShowPairlike a_a1ck[sk:1] b_a1cl[sk:1]
deeply_instantiate final subst
  origin: arising from a type equality forall a b.
                                       Show (Pair a b) =>
                                       ShowPairlike a b
                                       ~
                                       forall a b. Show (Pair a b) => ShowPairlike a b
  type: ShowPairlike a_a1cn[tau:1] b_a1co[tau:1]
  new type: ShowPairlike a_a1cn[tau:1] b_a1co[tau:1]
  subst: [TCvSubst
            In scope: InScope {a_a1cn b_a1co}
            Type env: []
            Co env: []]
u_tys
  tclvl 1
  ShowPairlike a_a1cn[tau:1] b_a1co[tau:1]
  ~
  ShowPairlike a_a1ck[sk:1] b_a1cl[sk:1]
  arising from a type equality ShowPairlike
                                 a_a1cn[tau:1] b_a1co[tau:1]
                               ~
                               ShowPairlike a_a1ck[sk:1] b_a1cl[sk:1]
u_tys
  tclvl 1
  a_a1cn[tau:1] ~ a_a1ck[sk:1]
  arising from a type equality ShowPairlike
                                 a_a1cn[tau:1] b_a1co[tau:1]
                               ~
                               ShowPairlike a_a1ck[sk:1] b_a1cl[sk:1]
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    a_a1cn[tau:1] ~ a_a1ck[sk:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    a_a1cn[tau:1] ~ a_a1ck[sk:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  a_a1cn[tau:1] :: *
  a_a1ck[sk:1] :: *
  True
  <*>_N
writeMetaTyVar a_a1cn[tau:1] := a_a1ck[sk:1]
u_tys yields no coercion
u_tys
  tclvl 1
  b_a1co[tau:1] ~ b_a1cl[sk:1]
  arising from a type equality ShowPairlike
                                 a_a1cn[tau:1] b_a1co[tau:1]
                               ~
                               ShowPairlike a_a1ck[sk:1] b_a1cl[sk:1]
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    b_a1co[tau:1] ~ b_a1cl[sk:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    b_a1co[tau:1] ~ b_a1cl[sk:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  b_a1co[tau:1] :: *
  b_a1cl[sk:1] :: *
  True
  <*>_N
writeMetaTyVar b_a1co[tau:1] := b_a1cl[sk:1]
u_tys yields no coercion
u_tys yields no coercion
pushLevelAndCaptureConstraints } 1
newTcEvBinds unique = a1cq
checkConstraints
  1
  [a_a1ck[sk:1], b_a1cl[sk:1]]
simplifyAmbiguityCheck {
  type =  forall a b. Show (Pair a b) => ShowPairlike a b
  wanted =  WC {wc_impl =
                  Implic {
                    TcLevel = 1
                    Skolems = a_a1ck[sk:1] b_a1cl[sk:1]
                    No-eqs = False
                    Status = Unsolved
                    Given = $dShow_a1cm :: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
                    Wanted =
                      WC {wc_simple =
                            [WD] $dShow_a1cp {0}:: Show
                                                     (Pair
                                                        a_a1cn[tau:1]
                                                        b_a1co[tau:1]) (CNonCanonical)}
                    Binds = EvBindsVar<a1cq>
                    an instance declaration:
                      forall a b. Show (Pair a b) => ShowPairlike a b }}
newTcEvBinds unique = a1cr
solveWanteds {
  Level = 0
  WC {wc_impl =
        Implic {
          TcLevel = 1
          Skolems = a_a1ck[sk:1] b_a1cl[sk:1]
          No-eqs = False
          Status = Unsolved
          Given = $dShow_a1cm :: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1cp {0}:: Show
                                           (Pair a_a1cn[tau:1] b_a1co[tau:1]) (CNonCanonical)}
          Binds = EvBindsVar<a1cq>
          an instance declaration:
            forall a b. Show (Pair a b) => ShowPairlike a b }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 1
    Skolems = a_a1ck[sk:1] b_a1cl[sk:1]
    No-eqs = False
    Status = Unsolved
    Given = $dShow_a1cm :: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1cp {0}:: Show
                                     (Pair a_a1cn[tau:1] b_a1co[tau:1]) (CNonCanonical)}
    Binds = EvBindsVar<a1cq>
    an instance declaration:
      forall a b. Show (Pair a b) => ShowPairlike a b }
  Inerts {Unsolved goals = 0}
         Inert fsks = []
solveSimpleGivens {
  [[G] $dShow_a1cm {0}:: Show
                           (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CNonCanonical)]
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [G] $dShow_a1cm {0}:: Show
                                      (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] $dShow_a1cm {0}:: Show
                                        (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CNonCanonical)
canEvNC:cls Show [Pair a_a1ck[sk:1] b_a1cl[sk:1]]
Emitting fresh work
flatten_args { Pair a_a1ck[sk:1] b_a1cl[sk:1]
Unfilled tyvar a_a1ck[sk:1]
Unfilled tyvar b_a1cl[sk:1]
flatten } Pair a_a1ck[sk:1] b_a1cl[sk:1]
canClass
  [G] $dShow_a1cm {0}:: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
  Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
  ContinueWith [G] $dShow_a1cm {0}:: Show
                                       (Pair a_a1ck[sk:1] b_a1cl[sk:1])
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [G] $dShow_a1cm {0}:: Show
                                        (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [G] $dShow_a1cm {0}:: Show
                                        (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
doTopReact
  [G] $dShow_a1cm {0}:: Show
                          (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
try_fundeps
  [G] $dShow_a1cm {0}:: Show
                          (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [G] $dShow_a1cm {0}:: Show
                                                           (Pair
                                                              a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
addInertCan }
Step 1[l:1,d:0] Kept as inert:
    [G] $dShow_a1cm {0}:: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
End solver pipeline (kept as inert) }
  final_item = [G] $dShow_a1cm {0}:: Show
                                       (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
End solveSimpleGivens }
solveWanteds {
  Level = 1
  WC {wc_simple =
        [WD] $dShow_a1cp {0}:: Show
                                 (Pair a_a1cn[tau:1] b_a1co[tau:1]) (CNonCanonical)}
solveSimpleWanteds {
  {[WD] $dShow_a1cp {0}:: Show
                            (Pair a_a1cn[tau:1] b_a1co[tau:1]) (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [WD] $dShow_a1cp {0}:: Show
                                       (Pair a_a1cn[tau:1] b_a1co[tau:1]) (CNonCanonical)
  inerts = {Dictionaries = [G] $dShow_a1cm {0}:: Show
                                                   (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShow_a1cp {0}:: Show
                                         (Pair a_a1cn[tau:1] b_a1co[tau:1]) (CNonCanonical)
canEvNC:cls Show [Pair a_a1cn[tau:1] b_a1co[tau:1]]
flatten_args { Pair a_a1cn[tau:1] b_a1co[tau:1]
Following filled tyvar a_a1cn[tau:1] = a_a1ck[sk:1]
Unfilled tyvar a_a1ck[sk:1]
Following filled tyvar b_a1co[tau:1] = b_a1cl[sk:1]
Unfilled tyvar b_a1cl[sk:1]
flatten } Pair a_a1ck[sk:1] b_a1cl[sk:1]
canClass
  [WD] $dShow_a1cp {0}:: Show (Pair a_a1cn[tau:1] b_a1co[tau:1])
  Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
  ContinueWith [WD] $dShow_a1cp {0}:: Show
                                        (Pair a_a1ck[sk:1] b_a1cl[sk:1])
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] $dShow_a1cp {0}:: Show
                                         (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
lookupInertDict keep inert
addTcEvBind
  a1cq
  [W] $dShow_a1cp = $dShow_a1cm
end stage interact with inerts }
Step 2[l:1,d:0] Dict equal (keep inert):
    [WD] $dShow_a1cp {0}:: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {[W] $dShow_a1cp = $dShow_a1cm}
getNoGivenEqs
  No given equalities
  Skols: [a_a1ck[sk:1], b_a1cl[sk:1]]
  Inerts: {Dictionaries = [G] $dShow_a1cm {0}:: Show
                                                  (Pair a_a1ck[sk:1] b_a1cl[sk:1]) (CDictCan)
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = [a_a1ck[sk:1], b_a1cl[sk:1]]
  Extended skols = {a_a1ck[sk:1], b_a1cl[sk:1], $dShow_a1cm,
                    $dShow_a1cp}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 1
    Skolems = a_a1ck[sk:1] b_a1cl[sk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShow_a1cm :: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
    Wanted = WC {}
    Binds = EvBindsVar<a1cq>
    an instance declaration:
      forall a b. Show (Pair a b) => ShowPairlike a b }
neededEvVars
  old_needs: {}
  seeds3: {$dShow_a1cm}
  tcvs: {}
  ev_binds: [a1cp :-> [W] $dShow_a1cp = $dShow_a1cm]
  live_ev_binds: [a1cp :-> [W] $dShow_a1cp = $dShow_a1cm]
setImplicationStatus(all-solved) }
  discard: True
  new_implic: Implic {
                TcLevel = 1
                Skolems = a_a1ck[sk:1] b_a1cl[sk:1]
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given = $dShow_a1cm :: Show (Pair a_a1ck[sk:1] b_a1cl[sk:1])
                Wanted = WC {}
                Binds = EvBindsVar<a1cq>
                an instance declaration:
                  forall a b. Show (Pair a b) => ShowPairlike a b }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Nothing
  implication evbinds = {[W] $dShow_a1cp = $dShow_a1cm}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {}
  unsolved_implics = {Nothing}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
Constraint solver steps = 2
unflattenGivens []
End simplifyAmbiguityCheck }
reportUnsolved(ambig) {
newTcEvBinds unique = a1cs
reportUnsolved(ambig) }
Done ambiguity check for
  forall a b. Show (Pair a b) => ShowPairlike a b
cvi 2 forall a b. Show (Pair a b) => ShowPairlike a b
End checkValidInstance }
tcLocalInstDecl 1
  forall a b. Show (Pair a b) => ShowPairlike a b
  3
  [a_a1ct[sk:1], b_a1cu[sk:1]]
tc_extend_local_env
  [(a_a1bj, Type variable ‘a_a1bj’ = a_a1ct[sk:1] :: *),
   (b_a1bk, Type variable ‘b_a1bk’ = b_a1cu[sk:1] :: *)]
tcExtendBinderStack [a_a1bj a_a1ct[sk:1], b_a1bk b_a1cu[sk:1]]
Adding instances:
  Foo.$fShowPairlikeab :
    instance Show (Pair a b) => ShowPairlike a b
      -- Defined at test-nested.hs:13:10
addFamInsts
tcAddTyCons
  tycons []
  implicits []
tcExtendKindEnvList []
tc_extend_local_env []
---- tcTyClGroup ---- {
Decls for [ShowFst]
tcExtendKindEnv [rs9 :-> APromotionErr ClassPE]
---- kcTyClGroup ---- {
  module Foo
  class (forall b_a1bb.
         Show b_a1bb => ShowPairlike a_a1ba b_a1bb) => ShowFst a_a1ba
getInitialKinds {
getInitialKinds done }
tcExtendKindEnvList []
solveEqualities { level = 1
getInitialKinds {
bindImplicitTKBndrs
  []
  []
tc_extend_local_env []
tcExtendBinderStack []
bindExplicTKBndrs [a_a1ba]
newMetaKindVar k_a1cy[tau:1]
newTyVarTyVar a_a1cz[tyv:1]
tc_extend_local_env
  [(a_a1ba, Type variable ‘a_a1ba’ = a_a1cz[tyv:1] :: k_a1cy[tau:1])]
tcExtendBinderStack [a_a1ba a_a1cz[tyv:1]]
kcLHsQTyVars: not-cusk
  ShowFst
  []
  [a_a1ba]
  []
  [a_a1cz[tyv:1]]
  k_a1cy[tau:1] -> Constraint
tc_extend_local_env
  [(a_a1ba, Type variable ‘a_a1ba’ = a_a1cz[tyv:1] :: k_a1cy[tau:1])]
tcExtendBinderStack [a_a1ba a_a1cz[tyv:1]]
getInitialKinds done }
kcTyClGroup: initial kinds ShowFst :: k_a1cy[tau:1] -> Constraint
tcExtendKindEnvList
  [(ShowFst, ATcTyCon ShowFst[tc] :: k_a1cy[tau:1] -> Constraint)]
kcTyClDecl { ShowFst
bindTyClTyVars
  ShowFst [anon-vis (a_a1cz[tyv:1])]
  [(a_a1ba, a_a1cz[tyv:1])]
tc_extend_local_env
  [(a_a1ba, Type variable ‘a_a1ba’ = a_a1cz[tyv:1] :: k_a1cy[tau:1])]
tcExtendBinderStack [a_a1ba a_a1cz[tyv:1]]
pushLevelAndCaptureConstraints { 2
bindExplicTKBndrs [b_a1bb]
newMetaKindVar k_a1cA[tau:2]
tc_extend_local_env
  [(b_a1bb, Type variable ‘b_a1bb’ = b_a1bb[sk:2] :: k_a1cA[tau:2])]
tcExtendBinderStack [b_a1bb b_a1bb[sk:2]]
lk1 Show
tcInferApps {
  Show
  [b_a1bb]
tcInferApps (vis normal app)
  [vis] *
  b_a1bb
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bb
tcInferApps {
  b_a1bb
  []
tcInferApps } b_a1bb[sk:2] :: k_a1cA[tau:2]
checkExpectedKind
  b_a1bb[sk:2]
  k_a1cA[tau:2]
checkExpectedKindX
  b_a1bb
  act_kind': k_a1cA[tau:2]
  exp_kind: *
u_tys
  tclvl 2
  k_a1cA[tau:2] ~ *
  arising from a type equality k_a1cA[tau:2] ~ *
u_tys
  tclvl 2
  * ~ *
  arising from a kind equality arising from k_a1cA[tau:2] ~ *
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1cA[tau:2] ~ *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1cA[tau:2] :: *
  * :: *
  True
  <*>_N
writeMetaTyVar k_a1cA[tau:2] := *
u_tys yields no coercion
checkExpectedKind
  k_a1cA[tau:2]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps } Show b_a1bb[sk:2] :: Constraint
checkExpectedKind
  Show b_a1bb[sk:2]
  Constraint
checkExpectedKindX
  Show b_a1bb
  act_kind': Constraint
  exp_kind: Constraint
lk1 ShowPairlike
tcInferApps {
  ShowPairlike
  [a_a1ba, b_a1bb]
tcInferApps (vis normal app)
  [vis] *
  a_a1ba
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1ba
tcInferApps {
  a_a1ba
  []
tcInferApps } a_a1cz[tyv:1] :: k_a1cy[tau:1]
checkExpectedKind
  a_a1cz[tyv:1]
  k_a1cy[tau:1]
checkExpectedKindX
  a_a1ba
  act_kind': k_a1cy[tau:1]
  exp_kind: *
u_tys
  tclvl 2
  k_a1cy[tau:1] ~ *
  arising from a type equality k_a1cy[tau:1] ~ *
uUnfilledVar2 not ok
  k_a1cy[tau:1]
  *
New coercion hole: co_a1cB
utype_defer
  k_a1cy[tau:1]
  TYPE 'GHC.Types.LiftedRep
  arising from a type equality k_a1cy[tau:1] ~ *
  In the first argument of ‘ShowPairlike’, namely ‘a_a1ba’
  In the class declaration for ‘ShowFst’
utype_defer2 {co_a1cB}
u_tys yields coercion: {co_a1cB}
checkExpectedKind
  k_a1cy[tau:1]
  *
  {co_a1cB}
tcInferApps (vis normal app) 2 *
tcInferApps (vis normal app)
  [vis] *
  b_a1bb
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bb
tcInferApps {
  b_a1bb
  []
tcInferApps } b_a1bb[sk:2] :: k_a1cA[tau:2]
checkExpectedKind
  b_a1bb[sk:2]
  k_a1cA[tau:2]
checkExpectedKindX
  b_a1bb
  act_kind': k_a1cA[tau:2]
  exp_kind: *
u_tys
  tclvl 2
  k_a1cA[tau:2] ~ *
  arising from a type equality k_a1cA[tau:2] ~ *
found filled tyvar k_a1cA[tau:2] :-> *
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a type equality k_a1cA[tau:2] ~ *
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  k_a1cA[tau:2]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps }
  ShowPairlike
    (a_a1cz[tyv:1] |> {co_a1cB}) b_a1bb[sk:2] :: Constraint
checkExpectedKind
  ShowPairlike (a_a1cz[tyv:1] |> {co_a1cB}) b_a1bb[sk:2]
  Constraint
checkExpectedKindX
  ShowPairlike a_a1ba b_a1bb
  act_kind': Constraint
  exp_kind: Constraint
pushLevelAndCaptureConstraints } 2
newNoTcEvBinds unique = a1cC
kcTyClDecl done } ShowFst
solveEqualities: running solver
  wanted =  WC {wc_impl =
                  Implic {
                    TcLevel = 2
                    Skolems = (b_a1bb[sk:2] :: k_a1cA[tau:2])
                    No-eqs = True
                    Status = Unsolved
                    Given =
                    Wanted =
                      WC {wc_simple =
                            [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                                     GHC.Prim.~# * (CNonCanonical)}
                    Binds = CoEvBindsVar<a1cC>
                    ‘forall b_a1bb. Show b_a1bb => ShowPairlike a_a1ba b_a1bb’ }}
newNoTcEvBinds unique = a1cD
solveWanteds {
  Level = 1
  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems = (b_a1bb[sk:2] :: k_a1cA[tau:2])
          No-eqs = True
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                           GHC.Prim.~# * (CNonCanonical)}
          Binds = CoEvBindsVar<a1cC>
          ‘forall b_a1bb. Show b_a1bb => ShowPairlike a_a1ba b_a1bb’ }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 2
    Skolems = (b_a1bb[sk:2] :: k_a1cA[tau:2])
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                     GHC.Prim.~# * (CNonCanonical)}
    Binds = CoEvBindsVar<a1cC>
    ‘forall b_a1bb. Show b_a1bb => ShowPairlike a_a1ba b_a1bb’ }
  Inerts {Unsolved goals = 0}
         Inert fsks = []
solveWanteds {
  Level = 2
  WC {wc_simple =
        [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                 GHC.Prim.~# * (CNonCanonical)}
solveSimpleWanteds {
  {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                       GHC.Prim.~# * (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                         GHC.Prim.~# * (CNonCanonical)
canEvNC:eq
  k_a1cy[tau:1]
  *
can_eq_nc
  False
  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1] GHC.Prim.~# *
  nominal equality
  k_a1cy[tau:1]
  k_a1cy[tau:1]
  *
  *
flatten { FM_FlattenAll k_a1cy[tau:1]
Unfilled tyvar k_a1cy[tau:1]
flatten } k_a1cy[tau:1]
flatten { FM_FlattenAll *
flatten } *
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
doTopReact
  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1] GHC.Prim.~# * (CTyEqCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                                            GHC.Prim.~# * (CTyEqCan)
addInertCan }
Step 1[l:2,d:0] Kept as inert:
    [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1] GHC.Prim.~# *
End solver pipeline (kept as inert) }
  final_item = [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                        GHC.Prim.~# * (CTyEqCan)
getUnsolvedInerts
   tv eqs = {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                      GHC.Prim.~# * (CTyEqCan)}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs = [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                     GHC.Prim.~# * (CTyEqCan)}
Unflattening 1 {}
Unflattening 2
  {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1] GHC.Prim.~# * (CTyEqCan)}
Unflattening 3 {}
Unflattening done
  {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {wc_simple =
                   [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)}
solveWanteds }
  final wc = WC {wc_simple =
                   [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)}
  current evbinds  = {}
getNoGivenEqs
  No given equalities
  Skols: [b_a1bb[sk:2]]
  Inerts: {Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done:
  {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
floatEqualities
  Skols = [b_a1bb[sk:2]]
  Extended skols = {b_a1bb[sk:2]}
  Simples = {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                      GHC.Prim.~# * (CNonCanonical)}
  Candidate eqs = {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                            GHC.Prim.~# * (CNonCanonical)}
  Floated eqs = {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                          GHC.Prim.~# * (CNonCanonical)}
solveImplication 2
  {}
  WC {}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 2
    Skolems = (b_a1bb[sk:2] :: k_a1cA[tau:2])
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted = WC {}
    Binds = CoEvBindsVar<a1cC>
    ‘forall b_a1bb. Show b_a1bb => ShowPairlike a_a1ba b_a1bb’ }
neededEvVars
  old_needs: {}
  seeds3: {}
  tcvs: {}
  ev_binds: []
  live_ev_binds: []
setImplicationStatus(all-solved) }
  discard: True
  new_implic: Implic {
                TcLevel = 2
                Skolems = (b_a1bb[sk:2] :: k_a1cA[tau:2])
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given =
                Wanted = WC {}
                Binds = CoEvBindsVar<a1cC>
                ‘forall b_a1bb. Show b_a1bb => ShowPairlike a_a1ba b_a1bb’ }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                          GHC.Prim.~# * (CNonCanonical)}
  res_implic = Nothing
  implication evbinds = {}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                              GHC.Prim.~# * (CNonCanonical)}
  unsolved_implics = {Nothing}
simpl_loop iteration=0 (no new given superclasses = True, 1 simples to solve)
simpl_loop: wc =
  WC {wc_simple =
        [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                 GHC.Prim.~# * (CNonCanonical)}
solveSimpleWanteds {
  {[WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                            GHC.Prim.~# * (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                       GHC.Prim.~# * (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                         GHC.Prim.~# * (CNonCanonical)
canEvNC:eq
  k_a1cy[tau:1]
  *
can_eq_nc
  False
  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1] GHC.Prim.~# *
  nominal equality
  k_a1cy[tau:1]
  k_a1cy[tau:1]
  *
  *
flatten { FM_FlattenAll k_a1cy[tau:1]
Unfilled tyvar k_a1cy[tau:1]
flatten } k_a1cy[tau:1]
flatten { FM_FlattenAll *
flatten } *
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1]
                                         GHC.Prim.~# * (CTyEqCan)
Sneaky unification:
  Unifies: k_a1cy[tau:1] := *
  Coercion: k_a1cy[tau:1] ~ *
  Left Kind is: *
  Right Kind is: *
unifyTyVar k_a1cy[tau:1] := *
writeMetaTyVar k_a1cy[tau:1] := *
Filling coercion hole co_a1cB := <*>_N
end stage interact with inerts }
Step 2[l:1,d:0] Solved by unification:
    [WD] hole{co_a1cB} {0}:: k_a1cy[tau:1] GHC.Prim.~# *
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
Constraint solver steps = 2
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1cE
Skolemising a_a1cz[tyv:1] := a_a1cz[sk:1]
writeMetaTyVar a_a1cz[tyv:1] := a_a1cz[sk:1]
quantifyTyVars 1
  0
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
  {}
quantifyTyVars 2
  globals: {}
  mono_tvs: {}
  nondep:
  dep:
  dep_kvs'
  nondep_tvs'
generaliseTcTyCon: before zonkRec
  spec_req_tvs = a_a1cz[sk:1]
  inferred =
generaliseTcTyCon: post zonk
  tycon = ShowFst[tc]
  inferred =
  ze = ZE {ze_tv_env = [a1cz :-> a_a1ba]
           ze_id_env = []}
  spec_req_prs = [(a_a1ba, a_a1cz[tyv:1])]
  spec_req_tvs = a_a1cz[sk:1]
  final_spec_req_tvs = a_a1ba
generaliseTcTyCon done
  tycon = ShowFst[tc]
  tc_res_kind = Constraint
  dep_fv_set = {}
  final_spec_req_tvs = a_a1ba
  inferred =
  specified =
  required_tcbs = [anon-vis (a_a1ba)]
  final_tcbs = [anon-vis (a_a1ba)]
---- kcTyClGroup end ---- } ShowFst :: * -> Constraint
tcTyAndCl generalized kinds
  (ShowFst, [anon-vis (a_a1ba)], Constraint True)
tcExtendKindEnvList
  [(ShowFst, ATcTyCon ShowFst[tc] :: * -> Constraint)]
---- tcTyClDecl ---- {
  class (forall b_a1bb.
         Show b_a1bb => ShowPairlike a_a1ba b_a1bb) => ShowFst a_a1ba
bindTyClTyVars
  ShowFst [anon-vis (a_a1ba)]
  [(a_a1ba, a_a1ba)]
tc_extend_local_env
  [(a_a1ba, Type variable ‘a_a1ba’ = a_a1ba :: *)]
tcExtendBinderStack [a_a1ba a_a1ba]
tcClassDecl 1
  ShowFst
  [anon-vis (a_a1ba)]
solveEqualities { level = 1
pushLevelAndCaptureConstraints { 2
bindExplicTKBndrs [b_a1bb]
newMetaKindVar k_a1cF[tau:2]
tc_extend_local_env
  [(b_a1bb, Type variable ‘b_a1bb’ = b_a1bb[sk:2] :: k_a1cF[tau:2])]
tcExtendBinderStack [b_a1bb b_a1bb[sk:2]]
lk1 Show
tcInferApps {
  Show
  [b_a1bb]
tcInferApps (vis normal app)
  [vis] *
  b_a1bb
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bb
tcInferApps {
  b_a1bb
  []
tcInferApps } b_a1bb[sk:2] :: k_a1cF[tau:2]
checkExpectedKind
  b_a1bb[sk:2]
  k_a1cF[tau:2]
checkExpectedKindX
  b_a1bb
  act_kind': k_a1cF[tau:2]
  exp_kind: *
u_tys
  tclvl 2
  k_a1cF[tau:2] ~ *
  arising from a type equality k_a1cF[tau:2] ~ *
u_tys
  tclvl 2
  * ~ *
  arising from a kind equality arising from k_a1cF[tau:2] ~ *
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1cF[tau:2] ~ *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1cF[tau:2] :: *
  * :: *
  True
  <*>_N
writeMetaTyVar k_a1cF[tau:2] := *
u_tys yields no coercion
checkExpectedKind
  k_a1cF[tau:2]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps } Show b_a1bb[sk:2] :: Constraint
checkExpectedKind
  Show b_a1bb[sk:2]
  Constraint
checkExpectedKindX
  Show b_a1bb
  act_kind': Constraint
  exp_kind: Constraint
lk1 ShowPairlike
tcInferApps {
  ShowPairlike
  [a_a1ba, b_a1bb]
tcInferApps (vis normal app)
  [vis] *
  a_a1ba
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1ba
tcInferApps {
  a_a1ba
  []
tcInferApps } a_a1ba :: *
checkExpectedKind
  a_a1ba
  *
checkExpectedKindX
  a_a1ba
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps (vis normal app)
  [vis] *
  b_a1bb
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 b_a1bb
tcInferApps {
  b_a1bb
  []
tcInferApps } b_a1bb[sk:2] :: k_a1cF[tau:2]
checkExpectedKind
  b_a1bb[sk:2]
  k_a1cF[tau:2]
checkExpectedKindX
  b_a1bb
  act_kind': k_a1cF[tau:2]
  exp_kind: *
u_tys
  tclvl 2
  k_a1cF[tau:2] ~ *
  arising from a type equality k_a1cF[tau:2] ~ *
found filled tyvar k_a1cF[tau:2] :-> *
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a type equality k_a1cF[tau:2] ~ *
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  k_a1cF[tau:2]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps } ShowPairlike a_a1ba b_a1bb[sk:2] :: Constraint
checkExpectedKind
  ShowPairlike a_a1ba b_a1bb[sk:2]
  Constraint
checkExpectedKindX
  ShowPairlike a_a1ba b_a1bb
  act_kind': Constraint
  exp_kind: Constraint
pushLevelAndCaptureConstraints } 2
tcClassSigs 1 ShowFst
tcClassSigs 2 ShowFst
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1cG
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1cH
tcClassDecl
  []
  [anon-vis (a_a1ba)]
  []
---- tcTyClDecl end ---- } ShowFst
Starting synonym cycle check [ShowFst]
Done synonym cycle check [ShowFst]
Starting validity check [ShowFst]
Starting validity for tycon ShowFst
checkValidTyCon
  ShowFst
  Just ShowFst
check_valid_theta [forall b. Show b => ShowPairlike a_a1ba b]
check_type
  forall b. Show b => ShowPairlike a_a1ba b
  True
check_valid_theta [Show b_a1bb]
Done validity for tycon ShowFst
Done validity check [ShowFst]
---- end tcTyClGroup ---- }
tcAddTyCons
  tycons [ShowFst]
  implicits [Identifier ‘Foo.$p1ShowFst’,
             Coercion axiom ‘Foo.N:ShowFst’, Data constructor ‘Foo.C:ShowFst’,
             Identifier ‘Foo.C:ShowFst’]
tcExtendKindEnvList []
tc_extend_local_env []
Adding instances:
addFamInsts
tcAddTyCons
  tycons []
  implicits []
tcExtendKindEnvList []
tc_extend_local_env []
---- tcTyClGroup ---- {
Decls for [PairWithInt]
tcExtendKindEnv
  [rq9 :-> APromotionErr TyConPE, rs8 :-> APromotionErr RecDataConPE]
---- kcTyClGroup ---- {
  module Foo
  data PairWithInt a_aGu = MkPairWithInt (Pair a_aGu Int)
getInitialKinds {
getInitialKinds done }
tcExtendKindEnvList []
solveEqualities { level = 1
getInitialKinds {
bindImplicitTKBndrs
  []
  []
tc_extend_local_env []
tcExtendBinderStack []
bindExplicTKBndrs [a_aGu]
newMetaKindVar k_a1cQ[tau:1]
newTyVarTyVar a_a1cR[tyv:1]
tc_extend_local_env
  [(a_aGu, Type variable ‘a_aGu’ = a_a1cR[tyv:1] :: k_a1cQ[tau:1])]
tcExtendBinderStack [a_aGu a_a1cR[tyv:1]]
kcLHsQTyVars: not-cusk
  PairWithInt
  []
  [a_aGu]
  []
  [a_a1cR[tyv:1]]
  k_a1cQ[tau:1] -> *
getInitialKinds done }
kcTyClGroup: initial kinds PairWithInt :: k_a1cQ[tau:1] -> *
tcExtendKindEnvList
  [(PairWithInt, ATcTyCon PairWithInt[tc] :: k_a1cQ[tau:1] -> *)]
kcTyClDecl { PairWithInt
bindTyClTyVars
  PairWithInt [anon-vis (a_a1cR[tyv:1])]
  [(a_aGu, a_a1cR[tyv:1])]
tc_extend_local_env
  [(a_aGu, Type variable ‘a_aGu’ = a_a1cR[tyv:1] :: k_a1cQ[tau:1])]
tcExtendBinderStack [a_aGu a_a1cR[tyv:1]]
bindExplicTKBndrs []
kcConDecl {
  MkPairWithInt
  PrefixCon [(Pair a_aGu Int)]
newAnonMetaTyVar t_a1cS[tau:1]
lk1 Pair
tcInferApps {
  Pair
  [a_aGu, Int]
tcInferApps (vis normal app)
  [vis] *
  a_aGu
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_aGu
tcInferApps {
  a_aGu
  []
tcInferApps } a_a1cR[tyv:1] :: k_a1cQ[tau:1]
checkExpectedKind
  a_a1cR[tyv:1]
  k_a1cQ[tau:1]
checkExpectedKindX
  a_aGu
  act_kind': k_a1cQ[tau:1]
  exp_kind: *
u_tys
  tclvl 1
  k_a1cQ[tau:1] ~ *
  arising from a type equality k_a1cQ[tau:1] ~ *
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from k_a1cQ[tau:1] ~ *
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1cQ[tau:1] ~ *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1cQ[tau:1] :: *
  * :: *
  True
  <*>_N
writeMetaTyVar k_a1cQ[tau:1] := *
u_tys yields no coercion
checkExpectedKind
  k_a1cQ[tau:1]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps (vis normal app)
  [vis] *
  Int
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 Int
tcInferApps {
  Int
  []
tcInferApps } Int :: *
checkExpectedKind
  Int
  *
checkExpectedKindX
  Int
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps } Pair a_a1cR[tyv:1] Int :: *
checkExpectedKind
  Pair a_a1cR[tyv:1] Int
  *
checkExpectedKindX
  Pair a_aGu Int
  act_kind': *
  exp_kind: TYPE t_a1cS[tau:1]
u_tys
  tclvl 1
  * ~ TYPE t_a1cS[tau:1]
  arising from a type equality * ~ TYPE t_a1cS[tau:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ t_a1cS[tau:1]
  arising from a type equality * ~ TYPE t_a1cS[tau:1]
u_tys
  tclvl 1
  GHC.Types.RuntimeRep ~ GHC.Types.RuntimeRep
  arising from a kind equality arising from
    t_a1cS[tau:1] ~ 'GHC.Types.LiftedRep
u_tys yields no coercion
uUnfilledVar2 ok
  t_a1cS[tau:1] :: GHC.Types.RuntimeRep
  'GHC.Types.LiftedRep :: GHC.Types.RuntimeRep
  True
  <GHC.Types.RuntimeRep>_N
writeMetaTyVar t_a1cS[tau:1] := 'GHC.Types.LiftedRep
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  *
  TYPE t_a1cS[tau:1]
  <*>_N
kcConDecl } MkPairWithInt
kcTyClDecl done } PairWithInt
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1cT
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1cU
Skolemising a_a1cR[tyv:1] := a_a1cR[sk:1]
writeMetaTyVar a_a1cR[tyv:1] := a_a1cR[sk:1]
quantifyTyVars 1
  0
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
  {}
quantifyTyVars 2
  globals: {}
  mono_tvs: {}
  nondep:
  dep:
  dep_kvs'
  nondep_tvs'
generaliseTcTyCon: before zonkRec
  spec_req_tvs = a_a1cR[sk:1]
  inferred =
generaliseTcTyCon: post zonk
  tycon = PairWithInt[tc]
  inferred =
  ze = ZE {ze_tv_env = [a1cR :-> a_aGu]
           ze_id_env = []}
  spec_req_prs = [(a_aGu, a_a1cR[tyv:1])]
  spec_req_tvs = a_a1cR[sk:1]
  final_spec_req_tvs = a_aGu
generaliseTcTyCon done
  tycon = PairWithInt[tc]
  tc_res_kind = *
  dep_fv_set = {}
  final_spec_req_tvs = a_aGu
  inferred =
  specified =
  required_tcbs = [anon-vis (a_aGu)]
  final_tcbs = [anon-vis (a_aGu)]
---- kcTyClGroup end ---- } PairWithInt :: * -> *
tcTyAndCl generalized kinds
  (PairWithInt, [anon-vis (a_aGu)], * True)
tcExtendKindEnvList
  [(PairWithInt, ATcTyCon PairWithInt[tc] :: * -> *)]
---- tcTyClDecl ---- {
  data PairWithInt a_aGu = MkPairWithInt (Pair a_aGu Int)
bindTyClTyVars
  PairWithInt [anon-vis (a_aGu)]
  [(a_aGu, a_aGu)]
tc_extend_local_env [(a_aGu, Type variable ‘a_aGu’ = a_aGu :: *)]
tcExtendBinderStack [a_aGu a_aGu]
solveEqualities { level = 1
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1cW
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1cX
tcConDecl 1
  MkPairWithInt
  []
solveEqualities { level = 1
bindExplicTKBndrs []
tcConArg 1 (Pair a_aGu Int)
newAnonMetaTyVar t_a1cY[tau:1]
lk1 Pair
tcInferApps {
  Pair
  [a_aGu, Int]
tcInferApps (vis normal app)
  [vis] *
  a_aGu
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_aGu
tcInferApps {
  a_aGu
  []
tcInferApps } a_aGu :: *
checkExpectedKind
  a_aGu
  *
checkExpectedKindX
  a_aGu
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps (vis normal app)
  [vis] *
  Int
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 Int
tcInferApps {
  Int
  []
tcInferApps } Int :: *
checkExpectedKind
  Int
  *
checkExpectedKindX
  Int
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps } Pair a_aGu Int :: *
checkExpectedKind
  Pair a_aGu Int
  *
checkExpectedKindX
  Pair a_aGu Int
  act_kind': *
  exp_kind: TYPE t_a1cY[tau:1]
u_tys
  tclvl 1
  * ~ TYPE t_a1cY[tau:1]
  arising from a type equality * ~ TYPE t_a1cY[tau:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ t_a1cY[tau:1]
  arising from a type equality * ~ TYPE t_a1cY[tau:1]
u_tys
  tclvl 1
  GHC.Types.RuntimeRep ~ GHC.Types.RuntimeRep
  arising from a kind equality arising from
    t_a1cY[tau:1] ~ 'GHC.Types.LiftedRep
u_tys yields no coercion
uUnfilledVar2 ok
  t_a1cY[tau:1] :: GHC.Types.RuntimeRep
  'GHC.Types.LiftedRep :: GHC.Types.RuntimeRep
  True
  <GHC.Types.RuntimeRep>_N
writeMetaTyVar t_a1cY[tau:1] := 'GHC.Types.LiftedRep
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  *
  TYPE t_a1cY[tau:1]
  <*>_N
tcConArg 2 (Pair a_aGu Int)
lookupCF
  MkPairWithInt
  Nothing
  []
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1cZ
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1d0
kindGeneralise1 forall a. Pair a Int -> ()
kindGeneralize
  forall a. Pair a Int -> ()
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
quantifyTyVars 1
  0
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
  {a_aGu}
quantifyTyVars 2
  globals: {a_aGu}
  mono_tvs: {a_aGu}
  nondep:
  dep:
  dep_kvs'
  nondep_tvs'
tcConDecl 2
  MkPairWithInt
  []
tcConDecl 2 MkPairWithInt
tcDataDefn
  PairWithInt
  [anon-vis (a_aGu)]
  []
---- tcTyClDecl end ---- } PairWithInt
Starting synonym cycle check [PairWithInt]
Done synonym cycle check [PairWithInt]
Starting validity check [PairWithInt]
Starting validity for tycon PairWithInt
checkValidTyCon
  PairWithInt
  Nothing
cvtc1 PairWithInt
cvtc2 PairWithInt
checkValidDataCon
  MkPairWithInt
  PairWithInt
  [a_aGu]
  PairWithInt a_aGu :: *
  PairWithInt a_aGu :: *
checkValidDataCon 2 forall a. Pair a Int -> PairWithInt a
checkValidType forall a. Pair a Int -> PairWithInt a :: *
check_type
  forall a. Pair a Int -> PairWithInt a
  True
done ct forall a. Pair a Int -> PairWithInt a
Ambiguity check for forall a. Pair a Int -> PairWithInt a
tcSubType_NC
  the type of the constructor ‘MkPairWithInt’
  forall a. Pair a Int -> PairWithInt a
  forall a. Pair a Int -> PairWithInt a
tc_sub_tc_type (general case)
  ty_actual   = forall a. Pair a Int -> PairWithInt a
  ty_expected = forall a. Pair a Int -> PairWithInt a
tcSkolemise
tcSkolemise
  0
  expected_ty forall a. Pair a Int -> PairWithInt a
  inst tyvars [(a_aGu, a_a1d7[sk:1])]
  given []
  inst type Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
pushLevelAndCaptureConstraints { 1
tc_sub_type_ds
  ty_actual   = forall a. Pair a Int -> PairWithInt a
  ty_expected = Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
cloneAnonMetaTyVar a_a1d8[tau:1]
Instantiating
  all tyvars? True
  origin arising from a type equality forall a.
                                      Pair a Int -> PairWithInt a
                                      ~
                                      forall a. Pair a Int -> PairWithInt a
  type forall @a_aGu. Pair a_aGu Int -> PairWithInt a_aGu
  theta []
  leave_bndrs []
  with a_a1d8[tau:1]
  theta: []
tc_sub_type_ds
  ty_actual   = Pair a_a1d8[tau:1] Int -> PairWithInt a_a1d8[tau:1]
  ty_expected = Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
tc_sub_type_ds
  ty_actual   = PairWithInt a_a1d8[tau:1]
  ty_expected = PairWithInt a_a1d7[sk:1]
deeply_instantiate final subst
  origin: arising from a type equality forall a.
                                       Pair a Int -> PairWithInt a
                                       ~
                                       forall a. Pair a Int -> PairWithInt a
  type: PairWithInt a_a1d8[tau:1]
  new type: PairWithInt a_a1d8[tau:1]
  subst: [TCvSubst
            In scope: InScope {a_a1d8}
            Type env: []
            Co env: []]
u_tys
  tclvl 1
  PairWithInt a_a1d8[tau:1] ~ PairWithInt a_a1d7[sk:1]
  arising from a type equality Pair a_a1d8[tau:1] Int
                               -> PairWithInt a_a1d8[tau:1]
                               ~
                               Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
u_tys
  tclvl 1
  a_a1d8[tau:1] ~ a_a1d7[sk:1]
  arising from a type equality Pair a_a1d8[tau:1] Int
                               -> PairWithInt a_a1d8[tau:1]
                               ~
                               Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    a_a1d8[tau:1] ~ a_a1d7[sk:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    a_a1d8[tau:1] ~ a_a1d7[sk:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  a_a1d8[tau:1] :: *
  a_a1d7[sk:1] :: *
  True
  <*>_N
writeMetaTyVar a_a1d8[tau:1] := a_a1d7[sk:1]
u_tys yields no coercion
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = Pair a_a1d7[sk:1] Int
  ty_expected = Pair a_a1d8[tau:1] Int
tcSkolemise
tcSkolemise
  1
  expected_ty Pair a_a1d8[tau:1] Int
  inst tyvars []
  given []
  inst type Pair a_a1d8[tau:1] Int
tc_sub_type_ds
  ty_actual   = Pair a_a1d7[sk:1] Int
  ty_expected = Pair a_a1d8[tau:1] Int
deeply_instantiate final subst
  origin: arising from a type expected by the context:
                         Pair a_a1d7[sk:1] Int
  type: Pair a_a1d7[sk:1] Int
  new type: Pair a_a1d7[sk:1] Int
  subst: [TCvSubst
            In scope: InScope {a_a1d7}
            Type env: []
            Co env: []]
u_tys
  tclvl 1
  Pair a_a1d7[sk:1] Int ~ Pair a_a1d8[tau:1] Int
  arising from a type equality Pair a_a1d8[tau:1] Int
                               -> PairWithInt a_a1d8[tau:1]
                               ~
                               Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
u_tys
  tclvl 1
  a_a1d7[sk:1] ~ a_a1d8[tau:1]
  arising from a type equality Pair a_a1d8[tau:1] Int
                               -> PairWithInt a_a1d8[tau:1]
                               ~
                               Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
u_tys yields no coercion
u_tys
  tclvl 1
  Int ~ Int
  arising from a type equality Pair a_a1d8[tau:1] Int
                               -> PairWithInt a_a1d8[tau:1]
                               ~
                               Pair a_a1d7[sk:1] Int -> PairWithInt a_a1d7[sk:1]
u_tys yields no coercion
u_tys yields no coercion
pushLevelAndCaptureConstraints } 1
checkConstraints
  1
  [a_a1d7[sk:1]]
simplifyAmbiguityCheck {
  type =  forall a. Pair a Int -> PairWithInt a
  wanted =  WC {}
newTcEvBinds unique = a1d9
solveWanteds {
  Level = 0
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End simplifyAmbiguityCheck }
reportUnsolved(ambig) {
newTcEvBinds unique = a1da
reportUnsolved(ambig) }
Done ambiguity check for forall a. Pair a Int -> PairWithInt a
checkValidType done forall a. Pair a Int -> PairWithInt a :: *
Done validity of data con
  MkPairWithInt
  Datacon user type: forall a. Pair a Int -> PairWithInt a
  Datacon rep type: forall a. Pair a Int -> PairWithInt a
  Rep typcon binders: [anon-vis (a_aGu)]
  not family
Done validity for tycon PairWithInt
Done validity check [PairWithInt]
---- end tcTyClGroup ---- }
tcAddTyCons
  tycons [PairWithInt]
  implicits [Data constructor ‘MkPairWithInt’,
             Identifier ‘Foo.MkPairWithInt’]
tcExtendKindEnvList []
tc_extend_local_env []
tcLocalInstDecl ShowFst a_a1bh => Show (PairWithInt a_a1bh)
tcTopLHsType { ShowFst a_a1bh => Show (PairWithInt a_a1bh)
solveEqualities { level = 1
newMetaKindVar k_a1db[tau:1]
bindImplicitTKBndrs
  [a_a1bh]
  [a_a1bh[sk:1]]
tc_extend_local_env
  [(a_a1bh, Type variable ‘a_a1bh’ = a_a1bh[sk:1] :: k_a1db[tau:1])]
tcExtendBinderStack [a_a1bh a_a1bh[sk:1]]
lk1 ShowFst
tcInferApps {
  ShowFst
  [a_a1bh]
tcInferApps (vis normal app)
  [vis] *
  a_a1bh
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1bh
tcInferApps {
  a_a1bh
  []
tcInferApps } a_a1bh[sk:1] :: k_a1db[tau:1]
checkExpectedKind
  a_a1bh[sk:1]
  k_a1db[tau:1]
checkExpectedKindX
  a_a1bh
  act_kind': k_a1db[tau:1]
  exp_kind: *
u_tys
  tclvl 1
  k_a1db[tau:1] ~ *
  arising from a type equality k_a1db[tau:1] ~ *
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from k_a1db[tau:1] ~ *
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from k_a1db[tau:1] ~ *
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  k_a1db[tau:1] :: *
  * :: *
  True
  <*>_N
writeMetaTyVar k_a1db[tau:1] := *
u_tys yields no coercion
checkExpectedKind
  k_a1db[tau:1]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps } ShowFst a_a1bh[sk:1] :: Constraint
checkExpectedKind
  ShowFst a_a1bh[sk:1]
  Constraint
checkExpectedKindX
  ShowFst a_a1bh
  act_kind': Constraint
  exp_kind: Constraint
lk1 Show
tcInferApps {
  Show
  [(PairWithInt a_a1bh)]
tcInferApps (vis normal app)
  [vis] *
  (PairWithInt a_a1bh)
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 PairWithInt
tcInferApps {
  PairWithInt
  [a_a1bh]
tcInferApps (vis normal app)
  [vis] *
  a_a1bh
  *
  [TCvSubst In scope: InScope {} Type env: [] Co env: []]
lk1 a_a1bh
tcInferApps {
  a_a1bh
  []
tcInferApps } a_a1bh[sk:1] :: k_a1db[tau:1]
checkExpectedKind
  a_a1bh[sk:1]
  k_a1db[tau:1]
checkExpectedKindX
  a_a1bh
  act_kind': k_a1db[tau:1]
  exp_kind: *
u_tys
  tclvl 1
  k_a1db[tau:1] ~ *
  arising from a type equality k_a1db[tau:1] ~ *
found filled tyvar k_a1db[tau:1] :-> *
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a type equality k_a1db[tau:1] ~ *
u_tys yields no coercion
u_tys yields no coercion
checkExpectedKind
  k_a1db[tau:1]
  *
  <*>_N
tcInferApps (vis normal app) 2 *
tcInferApps } PairWithInt a_a1bh[sk:1] :: *
checkExpectedKind
  PairWithInt a_a1bh[sk:1]
  *
checkExpectedKindX
  PairWithInt a_a1bh
  act_kind': *
  exp_kind: *
tcInferApps (vis normal app) 2 *
tcInferApps } Show (PairWithInt a_a1bh[sk:1]) :: Constraint
checkExpectedKind
  Show (PairWithInt a_a1bh[sk:1])
  Constraint
checkExpectedKindX
  Show (PairWithInt a_a1bh)
  act_kind': Constraint
  exp_kind: Constraint
solveEqualities: running solver wanted =  WC {}
newNoTcEvBinds unique = a1dc
solveWanteds {
  Level = 1
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
End solveEqualities }
newNoTcEvBinds unique = a1dd
kindGeneralise1 forall a. ShowFst a => Show (PairWithInt a)
kindGeneralize
  forall a. ShowFst a => Show (PairWithInt a)
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
quantifyTyVars 1
  0
  DV {dv_kvs = {}, dv_tvs = {}, dv_cvs = {}}
  {}
quantifyTyVars 2
  globals: {}
  mono_tvs: {}
  nondep:
  dep:
  dep_kvs'
  nondep_tvs'
End tcTopLHsType }
  ShowFst a_a1bh => Show (PairWithInt a_a1bh)
  forall a. ShowFst a => Show (PairWithInt a)
checkValidInstance { forall a. ShowFst a => Show (PairWithInt a)
check_valid_theta [ShowFst a_a1bh]
Ambiguity check for forall a. ShowFst a => Show (PairWithInt a)
tcSubType_NC
  an instance declaration
  forall a. ShowFst a => Show (PairWithInt a)
  forall a. ShowFst a => Show (PairWithInt a)
tc_sub_tc_type (general case)
  ty_actual   = forall a. ShowFst a => Show (PairWithInt a)
  ty_expected = forall a. ShowFst a => Show (PairWithInt a)
tcSkolemise
tcSkolemise
  0
  expected_ty forall a. ShowFst a => Show (PairWithInt a)
  inst tyvars [(a_a1bh, a_a1df[sk:1])]
  given [$dShowFst_a1dg]
  inst type Show (PairWithInt a_a1df[sk:1])
pushLevelAndCaptureConstraints { 1
tc_sub_type_ds
  ty_actual   = forall a. ShowFst a => Show (PairWithInt a)
  ty_expected = Show (PairWithInt a_a1df[sk:1])
cloneAnonMetaTyVar a_a1dh[tau:1]
instCallConstraints [$dShowFst_a1di]
Instantiating
  all tyvars? True
  origin arising from a type equality forall a.
                                      ShowFst a =>
                                      Show (PairWithInt a)
                                      ~
                                      forall a. ShowFst a => Show (PairWithInt a)
  type forall @a_a1bh. ShowFst a_a1bh => Show (PairWithInt a_a1bh)
  theta [ShowFst a_a1bh]
  leave_bndrs []
  with a_a1dh[tau:1]
  theta: [ShowFst a_a1dh[tau:1]]
tc_sub_type_ds
  ty_actual   = Show (PairWithInt a_a1dh[tau:1])
  ty_expected = Show (PairWithInt a_a1df[sk:1])
deeply_instantiate final subst
  origin: arising from a type equality forall a.
                                       ShowFst a =>
                                       Show (PairWithInt a)
                                       ~
                                       forall a. ShowFst a => Show (PairWithInt a)
  type: Show (PairWithInt a_a1dh[tau:1])
  new type: Show (PairWithInt a_a1dh[tau:1])
  subst: [TCvSubst
            In scope: InScope {a_a1dh}
            Type env: []
            Co env: []]
u_tys
  tclvl 1
  Show (PairWithInt a_a1dh[tau:1]) ~ Show (PairWithInt a_a1df[sk:1])
  arising from a type equality Show (PairWithInt a_a1dh[tau:1])
                               ~
                               Show (PairWithInt a_a1df[sk:1])
u_tys
  tclvl 1
  PairWithInt a_a1dh[tau:1] ~ PairWithInt a_a1df[sk:1]
  arising from a type equality Show (PairWithInt a_a1dh[tau:1])
                               ~
                               Show (PairWithInt a_a1df[sk:1])
u_tys
  tclvl 1
  a_a1dh[tau:1] ~ a_a1df[sk:1]
  arising from a type equality Show (PairWithInt a_a1dh[tau:1])
                               ~
                               Show (PairWithInt a_a1df[sk:1])
u_tys
  tclvl 1
  * ~ *
  arising from a kind equality arising from
    a_a1dh[tau:1] ~ a_a1df[sk:1]
u_tys
  tclvl 1
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    a_a1dh[tau:1] ~ a_a1df[sk:1]
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  a_a1dh[tau:1] :: *
  a_a1df[sk:1] :: *
  True
  <*>_N
writeMetaTyVar a_a1dh[tau:1] := a_a1df[sk:1]
u_tys yields no coercion
u_tys yields no coercion
u_tys yields no coercion
pushLevelAndCaptureConstraints } 1
newTcEvBinds unique = a1dj
checkConstraints
  1
  [a_a1df[sk:1]]
simplifyAmbiguityCheck {
  type =  forall a. ShowFst a => Show (PairWithInt a)
  wanted =  WC {wc_impl =
                  Implic {
                    TcLevel = 1
                    Skolems = a_a1df[sk:1]
                    No-eqs = False
                    Status = Unsolved
                    Given = $dShowFst_a1dg :: ShowFst a_a1df[sk:1]
                    Wanted =
                      WC {wc_simple =
                            [WD] $dShowFst_a1di {0}:: ShowFst a_a1dh[tau:1] (CNonCanonical)}
                    Binds = EvBindsVar<a1dj>
                    an instance declaration:
                      forall a. ShowFst a => Show (PairWithInt a) }}
newTcEvBinds unique = a1dk
solveWanteds {
  Level = 0
  WC {wc_impl =
        Implic {
          TcLevel = 1
          Skolems = a_a1df[sk:1]
          No-eqs = False
          Status = Unsolved
          Given = $dShowFst_a1dg :: ShowFst a_a1df[sk:1]
          Wanted =
            WC {wc_simple =
                  [WD] $dShowFst_a1di {0}:: ShowFst a_a1dh[tau:1] (CNonCanonical)}
          Binds = EvBindsVar<a1dj>
          an instance declaration:
            forall a. ShowFst a => Show (PairWithInt a) }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 1
    Skolems = a_a1df[sk:1]
    No-eqs = False
    Status = Unsolved
    Given = $dShowFst_a1dg :: ShowFst a_a1df[sk:1]
    Wanted =
      WC {wc_simple =
            [WD] $dShowFst_a1di {0}:: ShowFst a_a1dh[tau:1] (CNonCanonical)}
    Binds = EvBindsVar<a1dj>
    an instance declaration:
      forall a. ShowFst a => Show (PairWithInt a) }
  Inerts {Unsolved goals = 0}
         Inert fsks = []
solveSimpleGivens {
  [[G] $dShowFst_a1dg {0}:: ShowFst a_a1df[sk:1] (CNonCanonical)]
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [G] $dShowFst_a1dg {0}:: ShowFst
                                         a_a1df[sk:1] (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] $dShowFst_a1dg {0}:: ShowFst
                                           a_a1df[sk:1] (CNonCanonical)
canEvNC:cls ShowFst [a_a1df[sk:1]]
addTcEvBind
  a1dj
  [G] df_a1dl = Foo.$p1ShowFst @ a_a1df[sk:1] $dShowFst_a1dg
Emitting fresh work
  [G] df_a1dl {0}:: forall b.
                    Show b =>
                    ShowPairlike a_a1df[sk:1] b (CNonCanonical)
flatten_args { a_a1df[sk:1]
Unfilled tyvar a_a1df[sk:1]
flatten } a_a1df[sk:1]
canClass
  [G] $dShowFst_a1dg {0}:: ShowFst a_a1df[sk:1]
  ShowFst a_a1df[sk:1]
  ContinueWith [G] $dShowFst_a1dg {0}:: ShowFst a_a1df[sk:1]
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [G] $dShowFst_a1dg {0}:: ShowFst
                                           a_a1df[sk:1] (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [G] $dShowFst_a1dg {0}:: ShowFst
                                           a_a1df[sk:1] (CDictCan)
doTopReact [G] $dShowFst_a1dg {0}:: ShowFst a_a1df[sk:1] (CDictCan)
try_fundeps
  [G] $dShowFst_a1dg {0}:: ShowFst a_a1df[sk:1] (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [G] $dShowFst_a1dg {0}:: ShowFst
                                                              a_a1df[sk:1] (CDictCan)
addInertCan }
Step 1[l:1,d:0] Kept as inert:
    [G] $dShowFst_a1dg {0}:: ShowFst a_a1df[sk:1]
End solver pipeline (kept as inert) }
  final_item = [G] $dShowFst_a1dg {0}:: ShowFst
                                          a_a1df[sk:1] (CDictCan)
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [G] df_a1dl {0}:: forall b.
                                Show b =>
                                ShowPairlike a_a1df[sk:1] b (CNonCanonical)
  inerts = {Dictionaries = [G] $dShowFst_a1dg {0}:: ShowFst
                                                      a_a1df[sk:1] (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] df_a1dl {0}:: forall b.
                                  Show b =>
                                  ShowPairlike a_a1df[sk:1] b (CNonCanonical)
canEvNC:forall ShowPairlike a_a1df[sk:1] b_a1bb
flatten {
  FM_SubstOnly forall b. Show b => ShowPairlike a_a1df[sk:1] b
Unfilled tyvar b_a1bb
Unfilled tyvar a_a1df[sk:1]
Unfilled tyvar b_a1bb
flatten } forall b. Show b => ShowPairlike a_a1df[sk:1] b
end stage canonicalization }
Step 2[l:1,d:0] Given forall-constraint:
    [G] df_a1dl {0}:: forall b. Show b => ShowPairlike a_a1df[sk:1] b
End solver pipeline (discharged) }
End solveSimpleGivens }
solveWanteds {
  Level = 1
  WC {wc_simple =
        [WD] $dShowFst_a1di {0}:: ShowFst a_a1dh[tau:1] (CNonCanonical)}
solveSimpleWanteds {
  {[WD] $dShowFst_a1di {0}:: ShowFst a_a1dh[tau:1] (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [WD] $dShowFst_a1di {0}:: ShowFst
                                          a_a1dh[tau:1] (CNonCanonical)
  inerts = {Dictionaries = [G] $dShowFst_a1dg {0}:: ShowFst
                                                      a_a1df[sk:1] (CDictCan)
            Given instances = [G] df_a1dl {0}:: forall b.
                                                Show b =>
                                                ShowPairlike a_a1df[sk:1] b
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShowFst_a1di {0}:: ShowFst
                                            a_a1dh[tau:1] (CNonCanonical)
canEvNC:cls ShowFst [a_a1dh[tau:1]]
flatten_args { a_a1dh[tau:1]
Following filled tyvar a_a1dh[tau:1] = a_a1df[sk:1]
Unfilled tyvar a_a1df[sk:1]
flatten } a_a1df[sk:1]
canClass
  [WD] $dShowFst_a1di {0}:: ShowFst a_a1dh[tau:1]
  ShowFst a_a1df[sk:1]
  ContinueWith [WD] $dShowFst_a1di {0}:: ShowFst a_a1df[sk:1]
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] $dShowFst_a1di {0}:: ShowFst
                                            a_a1df[sk:1] (CDictCan(psc))
lookupInertDict keep inert
addTcEvBind
  a1dj
  [W] $dShowFst_a1di = $dShowFst_a1dg
end stage interact with inerts }
Step 3[l:1,d:0] Dict equal (keep inert):
    [WD] $dShowFst_a1di {0}:: ShowFst a_a1df[sk:1]
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {[G] df_a1dl
                        = Foo.$p1ShowFst @ a_a1df[sk:1] $dShowFst_a1dg,
                      [W] $dShowFst_a1di = $dShowFst_a1dg}
getNoGivenEqs
  No given equalities
  Skols: [a_a1df[sk:1]]
  Inerts: {Dictionaries = [G] $dShowFst_a1dg {0}:: ShowFst
                                                     a_a1df[sk:1] (CDictCan)
           Given instances = [G] df_a1dl {0}:: forall b.
                                               Show b =>
                                               ShowPairlike a_a1df[sk:1] b
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = [a_a1df[sk:1]]
  Extended skols = {a_a1df[sk:1], $dShowFst_a1dg, $dShowFst_a1di,
                    df_a1dl}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 1
    Skolems = a_a1df[sk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShowFst_a1dg :: ShowFst a_a1df[sk:1]
    Wanted = WC {}
    Binds = EvBindsVar<a1dj>
    an instance declaration:
      forall a. ShowFst a => Show (PairWithInt a) }
neededEvVars
  old_needs: {}
  seeds3: {$dShowFst_a1dg}
  tcvs: {}
  ev_binds: [a1dl :-> [G] df_a1dl
                        = Foo.$p1ShowFst @ a_a1df[sk:1] $dShowFst_a1dg,
             a1di :-> [W] $dShowFst_a1di = $dShowFst_a1dg]
  live_ev_binds: [a1di :-> [W] $dShowFst_a1di = $dShowFst_a1dg]
setImplicationStatus(all-solved) }
  discard: True
  new_implic: Implic {
                TcLevel = 1
                Skolems = a_a1df[sk:1]
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given = $dShowFst_a1dg :: ShowFst a_a1df[sk:1]
                Wanted = WC {}
                Binds = EvBindsVar<a1dj>
                an instance declaration:
                  forall a. ShowFst a => Show (PairWithInt a) }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Nothing
  implication evbinds = {[W] $dShowFst_a1di = $dShowFst_a1dg}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {}
  unsolved_implics = {Nothing}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
Constraint solver steps = 3
unflattenGivens []
End simplifyAmbiguityCheck }
reportUnsolved(ambig) {
newTcEvBinds unique = a1dm
reportUnsolved(ambig) }
Done ambiguity check for
  forall a. ShowFst a => Show (PairWithInt a)
cvi 2 forall a. ShowFst a => Show (PairWithInt a)
End checkValidInstance }
tcLocalInstDecl 1
  forall a. ShowFst a => Show (PairWithInt a)
  2
  [a_a1dn[sk:1]]
tc_extend_local_env
  [(a_a1bh, Type variable ‘a_a1bh’ = a_a1dn[sk:1] :: *)]
tcExtendBinderStack [a_a1bh a_a1dn[sk:1]]
Adding instances:
  Foo.$fShowPairWithInt :
    instance ShowFst a => Show (PairWithInt a)
      -- Defined at test-nested.hs:19:10
addFamInsts
tcAddTyCons
  tycons []
  implicits []
tcExtendKindEnvList []
tc_extend_local_env []
tcDeriving False
tcDeriving 1 []
rnd
Adding instances:
Tc3b
Tc3c
tcSemigroupWarnings
Tc4
Tc4a
Tc5
tcExtendKindEnvList []
tc_extend_local_env []
complete_matches
  []
  []
complete_matches []
tcExtendKindEnvList []
tc_extend_local_env []
complete_matches
  []
  []
complete_matches []
Tc6
tc_extend_local_env
  [(a_a1bc, Type variable ‘a_a1bc’ = a_a1bc[ssk:0] :: *),
   (b_a1bd, Type variable ‘b_a1bd’ = b_a1bd[ssk:0] :: *)]
tcExtendBinderStack [a_a1bc a_a1bc[ssk:0], b_a1bd b_a1bd[ssk:0]]
tc_extend_local_env
  [(a_a1ba, Type variable ‘a_a1ba’ = a_a1ba[ssk:0] :: *)]
tcExtendBinderStack [a_a1ba a_a1ba[ssk:0]]
tcInstDecl2
  [a_a1ds[ssk:1]]
  [PairWithInt a_a1ds[ssk:1]]
  [ShowFst a_a1ds[ssk:1]]
  []
newTcEvBinds unique = a1du
tc_extend_local_env
  [(a_a1bh, Type variable ‘a_a1bh’ = a_a1ds[ssk:1] :: *)]
tcExtendBinderStack [a_a1bh a_a1ds[ssk:1]]
tcInstMeth
  []
  {show (MkPairWithInt x_a1bi) = show x_a1bi}
tc_def showsPrec
tcMethodBody
  showsPrec forall a. Show a => Int -> a -> ShowS
  test-nested.hs:19:10-42
pushLevelAndCaptureConstraints { 2
tcPolyCheck
  showsPrec_a1dy
  test-nested.hs:19:10-42
tcExtendBinderStack [showsPrec_a1dz[<NotTopLevel>]]
tc_extend_local_env []
tcExtendBinderStack []
tcMatchesFun
  showsPrec_a1dz
  Check{Int -> PairWithInt a_a1ds[ssk:1] -> ShowS}
tcSkolemise
tcSkolemise
  2
  expected_ty Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
  inst tyvars []
  given []
  inst type Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
tcBody Check{Int -> PairWithInt a_a1ds[ssk:1] -> ShowS}
tcInferId
  GHC.Show.$dmshowsPrec :: forall a. Show a => Int -> a -> ShowS
tcFunApp
  GHC.Show.$dmshowsPrec :: forall a. Show a => Int -> a -> ShowS
  [@(PairWithInt a_a1ds[ssk:1])]
  Check{Int -> PairWithInt a_a1ds[ssk:1] -> ShowS}
Instantiating
  all tyvars? False
  origin arising from a use of ‘GHC.Show.$dmshowsPrec’
  type forall @a_a1dv. Show a_a1dv => Int -> a_a1dv -> ShowS
  theta [Show a_a1dv]
  leave_bndrs [@a_a1dv]
  with
  theta: []
solveLocalEqualitiesX { Called from tcHsTypeApp
tc_extend_local_env []
tcExtendBinderStack []
checkExpectedKind
  PairWithInt a_a1ds[ssk:1]
  *
checkExpectedKindX
  PairWithInt a_a1ds[ssk:1]
  act_kind': *
  exp_kind: *
solveLocalEqualities: running solver WC {}
newNoTcEvBinds unique = a1dB
solveWanteds {
  Level = 2
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
solveLocalEqualitiesX end } residual_wanted = WC {}
checkValidType PairWithInt a_a1ds[ssk:1] :: *
done ct PairWithInt a_a1ds[ssk:1]
checkValidType done PairWithInt a_a1ds[ssk:1] :: *
VTA
  a_a1dv
  TYPE 'GHC.Types.LiftedRep
  PairWithInt a_a1ds[ssk:1]
  TYPE 'GHC.Types.LiftedRep
  Show a_a1dv => Int -> a_a1dv -> ShowS
  Show (PairWithInt a_a1ds[ssk:1])
  => Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
tc_sub_type_ds
  ty_actual   = Show (PairWithInt a_a1ds[ssk:1]) =>
                Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
  ty_expected = Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
instCallConstraints [$dShow_a1dC]
Instantiating
  all tyvars? True
  origin arising from a use of ‘GHC.Show.$dmshowsPrec’
  type Show (PairWithInt a_a1ds[ssk:1])
       => Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
  theta [Show (PairWithInt a_a1ds[ssk:1])]
  leave_bndrs []
  with
  theta: [Show (PairWithInt a_a1ds[ssk:1])]
tc_sub_type_ds
  ty_actual   = Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
  ty_expected = Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
tc_sub_type_ds
  ty_actual   = PairWithInt a_a1ds[ssk:1] -> ShowS
  ty_expected = PairWithInt a_a1ds[ssk:1] -> ShowS
tc_sub_type_ds
  ty_actual   = ShowS
  ty_expected = ShowS
tc_sub_type_ds
  ty_actual   = String
  ty_expected = String
deeply_instantiate final subst
  origin: arising from a use of ‘GHC.Show.$dmshowsPrec’
  type: String
  new type: String
  subst: [TCvSubst In scope: InScope {} Type env: [] Co env: []]
u_tys
  tclvl 2
  String ~ String
  arising from a type equality Int
                               -> PairWithInt a_a1ds[ssk:1] -> ShowS
                               ~
                               Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = String
  ty_expected = String
tcSkolemise
tcSkolemise
  2
  expected_ty String
  inst tyvars []
  given []
  inst type String
tc_sub_type_ds
  ty_actual   = String
  ty_expected = String
deeply_instantiate final subst
  origin: arising from a type expected by the context:
                         String
  type: String
  new type: String
  subst: [TCvSubst In scope: InScope {} Type env: [] Co env: []]
u_tys
  tclvl 2
  String ~ String
  arising from a type equality Int
                               -> PairWithInt a_a1ds[ssk:1] -> ShowS
                               ~
                               Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = PairWithInt a_a1ds[ssk:1]
  ty_expected = PairWithInt a_a1ds[ssk:1]
tcSkolemise
tcSkolemise
  2
  expected_ty PairWithInt a_a1ds[ssk:1]
  inst tyvars []
  given []
  inst type PairWithInt a_a1ds[ssk:1]
tc_sub_type_ds
  ty_actual   = PairWithInt a_a1ds[ssk:1]
  ty_expected = PairWithInt a_a1ds[ssk:1]
deeply_instantiate final subst
  origin: arising from a type expected by the context:
                         PairWithInt a_a1ds[ssk:1]
  type: PairWithInt a_a1ds[ssk:1]
  new type: PairWithInt a_a1ds[ssk:1]
  subst: [TCvSubst
            In scope: InScope {a_a1ds}
            Type env: []
            Co env: []]
u_tys
  tclvl 2
  PairWithInt a_a1ds[ssk:1] ~ PairWithInt a_a1ds[ssk:1]
  arising from a type equality Int
                               -> PairWithInt a_a1ds[ssk:1] -> ShowS
                               ~
                               Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
u_tys
  tclvl 2
  a_a1ds[ssk:1] ~ a_a1ds[ssk:1]
  arising from a type equality Int
                               -> PairWithInt a_a1ds[ssk:1] -> ShowS
                               ~
                               Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
u_tys yields no coercion
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = Int
  ty_expected = Int
tcSkolemise
tcSkolemise
  2
  expected_ty Int
  inst tyvars []
  given []
  inst type Int
tc_sub_type_ds
  ty_actual   = Int
  ty_expected = Int
deeply_instantiate final subst
  origin: arising from a type expected by the context:
                         Int
  type: Int
  new type: Int
  subst: [TCvSubst In scope: InScope {} Type env: [] Co env: []]
u_tys
  tclvl 2
  Int ~ Int
  arising from a type equality Int
                               -> PairWithInt a_a1ds[ssk:1] -> ShowS
                               ~
                               Int -> PairWithInt a_a1ds[ssk:1] -> ShowS
u_tys yields no coercion
tcSpecPrags showsPrec_a1dy []
pushLevelAndCaptureConstraints } 2
newTcEvBinds unique = a1dD
tcSpecPrags $cshowsPrec_a1dx []
tcMethodBody
  show forall a. Show a => a -> String
  test-nested.hs:20:3-6
pushLevelAndCaptureConstraints { 2
tcPolyCheck
  show_a1dF
  test-nested.hs:20:3-6
tcExtendBinderStack [show_a1dG[<NotTopLevel>]]
tc_extend_local_env []
tcExtendBinderStack []
tcMatchesFun
  show_a1dG
  Check{PairWithInt a_a1ds[ssk:1] -> String}
tcSkolemise
tcSkolemise
  2
  expected_ty PairWithInt a_a1ds[ssk:1] -> String
  inst tyvars []
  given []
  inst type PairWithInt a_a1ds[ssk:1] -> String
tcConPat
  MkPairWithInt
  a_aGu
  []
  []
  [a_a1ds[ssk:1]]
  [Pair a_a1ds[ssk:1] Int]
  PrefixCon [x_a1bi]
tcPatBndr(not let)
  x_a1bi
  Pair a_a1ds[ssk:1] Int
tcExtendBinderStack [x_a1bi[<NotTopLevel>]]
tc_extend_local_env
  [(x_a1bi, Identifier[x_a1bi::Pair a_a1ds[ssk:1] Int, NotLetBound])]
tcBody Check{String}
tcInferId show :: forall a. Show a => a -> String
tcFunApp
  show :: forall a. Show a => a -> String
  [x_a1bi]
  Check{String}
cloneAnonMetaTyVar a_a1dH[tau:2]
instCallConstraints [$dShow_a1dI]
Instantiating
  all tyvars? True
  origin arising from a use of ‘show’
  type forall @a_a1bT. Show a_a1bT => a_a1bT -> String
  theta [Show a_a1bT]
  leave_bndrs []
  with a_a1dH[tau:2]
  theta: [Show a_a1dH[tau:2]]
tcPolyExprNC Check{a_a1dH[tau:2]}
tcSkolemise
tcSkolemise
  2
  expected_ty a_a1dH[tau:2]
  inst tyvars []
  given []
  inst type a_a1dH[tau:2]
tcInferId x_a1bi :: Pair a_a1ds[ssk:1] Int
tcCheckId
  x_a1bi
  Pair a_a1ds[ssk:1] Int
  Check{a_a1dH[tau:2]}
tcWrapResult
  Actual:   Pair a_a1ds[ssk:1] Int
  Expected: Check{a_a1dH[tau:2]}
tc_sub_type_ds
  ty_actual   = Pair a_a1ds[ssk:1] Int
  ty_expected = a_a1dH[tau:2]
deeply_instantiate final subst
  origin: arising from a use of ‘x_a1bi’
  type: Pair a_a1ds[ssk:1] Int
  new type: Pair a_a1ds[ssk:1] Int
  subst: [TCvSubst
            In scope: InScope {a_a1ds}
            Type env: []
            Co env: []]
u_tys
  tclvl 2
  Pair a_a1ds[ssk:1] Int ~ a_a1dH[tau:2]
  arising from a type equality Pair a_a1ds[ssk:1] Int ~ a_a1dH[tau:2]
u_tys
  tclvl 2
  * ~ *
  arising from a kind equality arising from
    a_a1dH[tau:2] ~ Pair a_a1ds[ssk:1] Int
u_tys
  tclvl 2
  'GHC.Types.LiftedRep ~ 'GHC.Types.LiftedRep
  arising from a kind equality arising from
    a_a1dH[tau:2] ~ Pair a_a1ds[ssk:1] Int
u_tys yields no coercion
u_tys yields no coercion
uUnfilledVar2 ok
  a_a1dH[tau:2] :: *
  Pair a_a1ds[ssk:1] Int :: *
  True
  <*>_N
writeMetaTyVar a_a1dH[tau:2] := Pair a_a1ds[ssk:1] Int
u_tys yields no coercion
tc_sub_type_ds
  ty_actual   = String
  ty_expected = String
deeply_instantiate final subst
  origin: arising from a use of ‘show’
  type: String
  new type: String
  subst: [TCvSubst In scope: InScope {} Type env: [] Co env: []]
u_tys
  tclvl 2
  String ~ String
  arising from a type equality String ~ String
u_tys yields no coercion
tcSpecPrags show_a1dF []
pushLevelAndCaptureConstraints } 2
newTcEvBinds unique = a1dJ
tcSpecPrags $cshow_a1dE []
tc_def showList
tcMethodBody
  showList forall a. Show a => [a] -> ShowS
  test-nested.hs:19:10-42
pushLevelAndCaptureConstraints { 2
tcPolyCheck
  showList_a1dM
  test-nested.hs:19:10-42
tcExtendBinderStack [showList_a1dN[<NotTopLevel>]]
tc_extend_local_env []
tcExtendBinderStack []
tcMatchesFun
  showList_a1dN
  Check{[PairWithInt a_a1ds[ssk:1]] -> ShowS}
tcSkolemise
tcSkolemise
  2
  expected_ty [PairWithInt a_a1ds[ssk:1]] -> ShowS
  inst tyvars []
  given []
  inst type [PairWithInt a_a1ds[ssk:1]] -> ShowS
tcBody Check{[PairWithInt a_a1ds[ssk:1]] -> ShowS}
tcInferId GHC.Show.$dmshowList :: forall a. Show a => [a] -> ShowS
tcFunApp
  GHC.Show.$dmshowList :: forall a. Show a => [a] -> ShowS
  [@(PairWithInt a_a1ds[ssk:1])]
  Check{[PairWithInt a_a1ds[ssk:1]] -> ShowS}
Instantiating
  all tyvars? False
  origin arising from a use of ‘GHC.Show.$dmshowList’
  type forall @a_a1dK. Show a_a1dK => [a_a1dK] -> ShowS
  theta [Show a_a1dK]
  leave_bndrs [@a_a1dK]
  with
  theta: []
solveLocalEqualitiesX { Called from tcHsTypeApp
tc_extend_local_env []
tcExtendBinderStack []
checkExpectedKind
  PairWithInt a_a1ds[ssk:1]
  *
checkExpectedKindX
  PairWithInt a_a1ds[ssk:1]
  act_kind': *
  exp_kind: *
solveLocalEqualities: running solver WC {}
newNoTcEvBinds unique = a1dO
solveWanteds {
  Level = 2
  WC {}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {}
unflattenGivens []
solveLocalEqualitiesX end } residual_wanted = WC {}
checkValidType PairWithInt a_a1ds[ssk:1] :: *
done ct PairWithInt a_a1ds[ssk:1]
checkValidType done PairWithInt a_a1ds[ssk:1] :: *
VTA
  a_a1dK
  TYPE 'GHC.Types.LiftedRep
  PairWithInt a_a1ds[ssk:1]
  TYPE 'GHC.Types.LiftedRep
  Show a_a1dK => [] a_a1dK -> ShowS
  Show (PairWithInt a_a1ds[ssk:1])
  => [] (PairWithInt a_a1ds[ssk:1]) -> ShowS
tc_sub_type_ds
  ty_actual   = Show (PairWithInt a_a1ds[ssk:1]) =>
                [PairWithInt a_a1ds[ssk:1]] -> ShowS
  ty_expected = [PairWithInt a_a1ds[ssk:1]] -> ShowS
instCallConstraints [$dShow_a1dP]
Instantiating
  all tyvars? True
  origin arising from a use of ‘GHC.Show.$dmshowList’
  type Show (PairWithInt a_a1ds[ssk:1])
       => [] (PairWithInt a_a1ds[ssk:1]) -> ShowS
  theta [Show (PairWithInt a_a1ds[ssk:1])]
  leave_bndrs []
  with
  theta: [Show (PairWithInt a_a1ds[ssk:1])]
tc_sub_type_ds
  ty_actual   = [PairWithInt a_a1ds[ssk:1]] -> ShowS
  ty_expected = [PairWithInt a_a1ds[ssk:1]] -> ShowS
tc_sub_type_ds
  ty_actual   = ShowS
  ty_expected = ShowS
tc_sub_type_ds
  ty_actual   = String
  ty_expected = String
deeply_instantiate final subst
  origin: arising from a use of ‘GHC.Show.$dmshowList’
  type: String
  new type: String
  subst: [TCvSubst In scope: InScope {} Type env: [] Co env: []]
u_tys
  tclvl 2
  String ~ String
  arising from a type equality [PairWithInt a_a1ds[ssk:1]] -> ShowS
                               ~
                               [PairWithInt a_a1ds[ssk:1]] -> ShowS
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = String
  ty_expected = String
tcSkolemise
tcSkolemise
  2
  expected_ty String
  inst tyvars []
  given []
  inst type String
tc_sub_type_ds
  ty_actual   = String
  ty_expected = String
deeply_instantiate final subst
  origin: arising from a type expected by the context:
                         String
  type: String
  new type: String
  subst: [TCvSubst In scope: InScope {} Type env: [] Co env: []]
u_tys
  tclvl 2
  String ~ String
  arising from a type equality [PairWithInt a_a1ds[ssk:1]] -> ShowS
                               ~
                               [PairWithInt a_a1ds[ssk:1]] -> ShowS
u_tys yields no coercion
tc_sub_tc_type (general case)
  ty_actual   = [PairWithInt a_a1ds[ssk:1]]
  ty_expected = [PairWithInt a_a1ds[ssk:1]]
tcSkolemise
tcSkolemise
  2
  expected_ty [PairWithInt a_a1ds[ssk:1]]
  inst tyvars []
  given []
  inst type [PairWithInt a_a1ds[ssk:1]]
tc_sub_type_ds
  ty_actual   = [PairWithInt a_a1ds[ssk:1]]
  ty_expected = [PairWithInt a_a1ds[ssk:1]]
deeply_instantiate final subst
  origin: arising from a type expected by the context:
                         [PairWithInt a_a1ds[ssk:1]]
  type: [PairWithInt a_a1ds[ssk:1]]
  new type: [PairWithInt a_a1ds[ssk:1]]
  subst: [TCvSubst
            In scope: InScope {a_a1ds}
            Type env: []
            Co env: []]
u_tys
  tclvl 2
  [PairWithInt a_a1ds[ssk:1]] ~ [PairWithInt a_a1ds[ssk:1]]
  arising from a type equality [PairWithInt a_a1ds[ssk:1]] -> ShowS
                               ~
                               [PairWithInt a_a1ds[ssk:1]] -> ShowS
u_tys
  tclvl 2
  PairWithInt a_a1ds[ssk:1] ~ PairWithInt a_a1ds[ssk:1]
  arising from a type equality [PairWithInt a_a1ds[ssk:1]] -> ShowS
                               ~
                               [PairWithInt a_a1ds[ssk:1]] -> ShowS
u_tys
  tclvl 2
  a_a1ds[ssk:1] ~ a_a1ds[ssk:1]
  arising from a type equality [PairWithInt a_a1ds[ssk:1]] -> ShowS
                               ~
                               [PairWithInt a_a1ds[ssk:1]] -> ShowS
u_tys yields no coercion
u_tys yields no coercion
u_tys yields no coercion
tcSpecPrags showList_a1dM []
pushLevelAndCaptureConstraints } 2
newTcEvBinds unique = a1dQ
tcSpecPrags $cshowList_a1dL []
tcInstDecl2
  [a_a1dS[ssk:1], b_a1dT[ssk:1]]
  [a_a1dS[ssk:1], b_a1dT[ssk:1]]
  [Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])]
  [forall (c :: * -> *).
   (c ~ Pair a_a1dS[ssk:1]) =>
   Show (c b_a1dT[ssk:1])]
newTcEvBinds unique = a1dV
pushLevelAndCaptureConstraints { 2
pushLevelAndCaptureConstraints } 2
newTcEvBinds unique = a1dX
addTcEvBind
  a1dX
  [W] df_a1dZ = df_a1dW
tc_extend_local_env
  [(a_a1bj, Type variable ‘a_a1bj’ = a_a1dS[ssk:1] :: *),
   (b_a1bk, Type variable ‘b_a1bk’ = b_a1dT[ssk:1] :: *)]
tcExtendBinderStack [a_a1bj a_a1dS[ssk:1], b_a1bk b_a1dT[ssk:1]]
tcInstMeth
  []
  {}
Tc7
Tc7a
checkMain not Main Foo
simplifyTop {
  wanted =  WC {wc_impl =
                  Implic {
                    TcLevel = 1
                    Skolems = a_a1ds[ssk:1]
                    No-eqs = False
                    Status = Unsolved
                    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
                    Wanted =
                      WC {wc_impl =
                            Implic {
                              TcLevel = 2
                              Skolems =
                              No-eqs = False
                              Status = Unsolved
                              Given =
                              Wanted =
                                WC {wc_simple =
                                      [WD] $dShow_a1dC {0}:: Show
                                                               (PairWithInt
                                                                  a_a1ds[ssk:1]) (CNonCanonical)}
                              Binds = EvBindsVar<a1dD>
                              the instance declaration }
                            Implic {
                              TcLevel = 2
                              Skolems =
                              No-eqs = False
                              Status = Unsolved
                              Given =
                              Wanted =
                                WC {wc_simple =
                                      [WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2] (CNonCanonical)}
                              Binds = EvBindsVar<a1dJ>
                              the instance declaration }
                            Implic {
                              TcLevel = 2
                              Skolems =
                              No-eqs = False
                              Status = Unsolved
                              Given =
                              Wanted =
                                WC {wc_simple =
                                      [WD] $dShow_a1dP {0}:: Show
                                                               (PairWithInt
                                                                  a_a1ds[ssk:1]) (CNonCanonical)}
                              Binds = EvBindsVar<a1dQ>
                              the instance declaration }}
                    Binds = EvBindsVar<a1du>
                    the instance declaration }
                  Implic {
                    TcLevel = 1
                    Skolems = a_a1dS[ssk:1] b_a1dT[ssk:1]
                    No-eqs = False
                    Status = Unsolved
                    Given = $dShow_a1dU :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
                    Wanted =
                      WC {wc_impl =
                            Implic {
                              TcLevel = 2
                              Skolems =
                              No-eqs = False
                              Status = Unsolved
                              Given =
                              Wanted =
                                WC {wc_simple =
                                      [WD] df_a1dW {0}:: forall (c :: * -> *).
                                                         (c ~ Pair a_a1dS[ssk:1]) =>
                                                         Show (c b_a1dT[ssk:1]) (CNonCanonical)}
                              Binds = EvBindsVar<a1dX>
                              the instance declaration }}
                    Binds = EvBindsVar<a1dV>
                    the instance declaration }}
newTcEvBinds unique = a1e1
solveWanteds {
  Level = 0
  WC {wc_impl =
        Implic {
          TcLevel = 1
          Skolems = a_a1ds[ssk:1]
          No-eqs = False
          Status = Unsolved
          Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
          Wanted =
            WC {wc_impl =
                  Implic {
                    TcLevel = 2
                    Skolems =
                    No-eqs = False
                    Status = Unsolved
                    Given =
                    Wanted =
                      WC {wc_simple =
                            [WD] $dShow_a1dC {0}:: Show
                                                     (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
                    Binds = EvBindsVar<a1dD>
                    the instance declaration }
                  Implic {
                    TcLevel = 2
                    Skolems =
                    No-eqs = False
                    Status = Unsolved
                    Given =
                    Wanted =
                      WC {wc_simple =
                            [WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2] (CNonCanonical)}
                    Binds = EvBindsVar<a1dJ>
                    the instance declaration }
                  Implic {
                    TcLevel = 2
                    Skolems =
                    No-eqs = False
                    Status = Unsolved
                    Given =
                    Wanted =
                      WC {wc_simple =
                            [WD] $dShow_a1dP {0}:: Show
                                                     (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
                    Binds = EvBindsVar<a1dQ>
                    the instance declaration }}
          Binds = EvBindsVar<a1du>
          the instance declaration }
        Implic {
          TcLevel = 1
          Skolems = a_a1dS[ssk:1] b_a1dT[ssk:1]
          No-eqs = False
          Status = Unsolved
          Given = $dShow_a1dU :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
          Wanted =
            WC {wc_impl =
                  Implic {
                    TcLevel = 2
                    Skolems =
                    No-eqs = False
                    Status = Unsolved
                    Given =
                    Wanted =
                      WC {wc_simple =
                            [WD] df_a1dW {0}:: forall (c :: * -> *).
                                               (c ~ Pair a_a1dS[ssk:1]) =>
                                               Show (c b_a1dT[ssk:1]) (CNonCanonical)}
                    Binds = EvBindsVar<a1dX>
                    the instance declaration }}
          Binds = EvBindsVar<a1dV>
          the instance declaration }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 1
    Skolems = a_a1ds[ssk:1]
    No-eqs = False
    Status = Unsolved
    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dC {0}:: Show
                                               (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
              Binds = EvBindsVar<a1dD>
              the instance declaration }
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2] (CNonCanonical)}
              Binds = EvBindsVar<a1dJ>
              the instance declaration }
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dP {0}:: Show
                                               (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
              Binds = EvBindsVar<a1dQ>
              the instance declaration }}
    Binds = EvBindsVar<a1du>
    the instance declaration }
  Inerts {Unsolved goals = 0}
         Inert fsks = []
solveSimpleGivens {
  [[G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1] (CNonCanonical)]
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [G] $dShowFst_a1dt {0}:: ShowFst
                                         a_a1ds[ssk:1] (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] $dShowFst_a1dt {0}:: ShowFst
                                           a_a1ds[ssk:1] (CNonCanonical)
canEvNC:cls ShowFst [a_a1ds[ssk:1]]
addTcEvBind
  a1du
  [G] df_a1e2 = Foo.$p1ShowFst @ a_a1ds[ssk:1] $dShowFst_a1dt
Emitting fresh work
  [G] df_a1e2 {0}:: forall b.
                    Show b =>
                    ShowPairlike a_a1ds[ssk:1] b (CNonCanonical)
flatten_args { a_a1ds[ssk:1]
Unfilled tyvar a_a1ds[ssk:1]
flatten } a_a1ds[ssk:1]
canClass
  [G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1]
  ShowFst a_a1ds[ssk:1]
  ContinueWith [G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1]
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [G] $dShowFst_a1dt {0}:: ShowFst
                                           a_a1ds[ssk:1] (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [G] $dShowFst_a1dt {0}:: ShowFst
                                           a_a1ds[ssk:1] (CDictCan)
doTopReact
  [G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1] (CDictCan)
try_fundeps
  [G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1] (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [G] $dShowFst_a1dt {0}:: ShowFst
                                                              a_a1ds[ssk:1] (CDictCan)
addInertCan }
Step 1[l:1,d:0] Kept as inert:
    [G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1]
End solver pipeline (kept as inert) }
  final_item = [G] $dShowFst_a1dt {0}:: ShowFst
                                          a_a1ds[ssk:1] (CDictCan)
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [G] df_a1e2 {0}:: forall b.
                                Show b =>
                                ShowPairlike a_a1ds[ssk:1] b (CNonCanonical)
  inerts = {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                      a_a1ds[ssk:1] (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] df_a1e2 {0}:: forall b.
                                  Show b =>
                                  ShowPairlike a_a1ds[ssk:1] b (CNonCanonical)
canEvNC:forall ShowPairlike a_a1ds[ssk:1] b_a1bb
flatten {
  FM_SubstOnly forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
Unfilled tyvar b_a1bb
Unfilled tyvar a_a1ds[ssk:1]
Unfilled tyvar b_a1bb
flatten } forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
end stage canonicalization }
Step 2[l:1,d:0] Given forall-constraint:
    [G] df_a1e2 {0}:: forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
End solver pipeline (discharged) }
End solveSimpleGivens }
solveWanteds {
  Level = 1
  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = False
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1dC {0}:: Show
                                           (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
          Binds = EvBindsVar<a1dD>
          the instance declaration }
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = False
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2] (CNonCanonical)}
          Binds = EvBindsVar<a1dJ>
          the instance declaration }
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = False
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1dP {0}:: Show
                                           (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
          Binds = EvBindsVar<a1dQ>
          the instance declaration }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = False
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dC {0}:: Show
                                     (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
    Binds = EvBindsVar<a1dD>
    the instance declaration }
  Inerts {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                    a_a1ds[ssk:1] (CDictCan)
          Given instances = [G] df_a1e2 {0}:: forall b.
                                              Show b =>
                                              ShowPairlike a_a1ds[ssk:1] b
          Unsolved goals = 0}
         Inert fsks = []
solveWanteds {
  Level = 2
  WC {wc_simple =
        [WD] $dShow_a1dC {0}:: Show
                                 (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
solveSimpleWanteds {
  {[WD] $dShow_a1dC {0}:: Show
                            (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] $dShow_a1dC {0}:: Show
                                       (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)
  inerts = {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                      a_a1ds[ssk:1] (CDictCan)
            Given instances = [G] df_a1e2 {0}:: forall b.
                                                Show b =>
                                                ShowPairlike a_a1ds[ssk:1] b
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShow_a1dC {0}:: Show
                                         (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)
canEvNC:cls Show [PairWithInt a_a1ds[ssk:1]]
flatten_args { PairWithInt a_a1ds[ssk:1]
Unfilled tyvar a_a1ds[ssk:1]
flatten } PairWithInt a_a1ds[ssk:1]
canClass
  [WD] $dShow_a1dC {0}:: Show (PairWithInt a_a1ds[ssk:1])
  Show (PairWithInt a_a1ds[ssk:1])
  ContinueWith [WD] $dShow_a1dC {0}:: Show
                                        (PairWithInt a_a1ds[ssk:1])
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] $dShow_a1dC {0}:: Show
                                         (PairWithInt a_a1ds[ssk:1]) (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] $dShow_a1dC {0}:: Show
                                         (PairWithInt a_a1ds[ssk:1]) (CDictCan)
doTopReact
  [WD] $dShow_a1dC {0}:: Show (PairWithInt a_a1ds[ssk:1]) (CDictCan)
matchClassInst pred = Show (PairWithInt a_a1ds[ssk:1]) {
matchInstEnv
  goal: Show [PairWithInt a_a1ds[ssk:1]]
  matches: [(instance ShowFst a => Show (PairWithInt a)
               -- Defined at test-nested.hs:19:10,
             [Just a_a1ds[ssk:1]])]
  unify: []
matchClass success
  dict Show (PairWithInt a_a1ds[ssk:1])
  witness Foo.$fShowPairWithInt forall a.
                                ShowFst a =>
                                Show (PairWithInt a)
match_one
  Foo.$fShowPairWithInt
  [Just a_a1ds[ssk:1]]
match_one 2
  Foo.$fShowPairWithInt
  [a_a1ds[ssk:1]]
  [ShowFst a_a1ds[ssk:1]]
} matchClassInst global result
  OneInst [ShowFst a_a1ds[ssk:1]]
          top-level instance [safe]
updSolvedSetTcs:
  [WD] $dShow_a1dC {0}:: Show (PairWithInt a_a1ds[ssk:1])
doTopReact/found instance for
  [WD] $dShow_a1dC {0}:: Show (PairWithInt a_a1ds[ssk:1])
newWantedEvVar/cache hit
  [G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1]
addTcEvBind
  a1dD
  [W] $dShow_a1dC
    = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt
end stage top-level reactions }
Step 3[l:2,d:0] Dict/Top (solved wanted):
    [WD] $dShow_a1dC {0}:: Show (PairWithInt a_a1ds[ssk:1])
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {[W] $dShow_a1dC
                        = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt}
getNoGivenEqs
  No given equalities
  Skols: []
  Inerts: {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                     a_a1ds[ssk:1] (CDictCan)
           Given instances = [G] df_a1e2 {0}:: forall b.
                                               Show b =>
                                               ShowPairlike a_a1ds[ssk:1] b
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = []
  Extended skols = {$dShow_a1dC}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted = WC {}
    Binds = EvBindsVar<a1dD>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {$dShowFst_a1dt, Foo.$fShowPairWithInt}
  tcvs: {}
  ev_binds: [a1dC :-> [W] $dShow_a1dC
                        = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt]
  live_ev_binds: [a1dC :-> [W] $dShow_a1dC
                             = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt]
setImplicationStatus(all-solved) }
  discard: False
  new_implic: Implic {
                TcLevel = 2
                Skolems =
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given =
                Wanted = WC {}
                Binds = EvBindsVar<a1dD>
                the instance declaration }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Just Implic {
                      TcLevel = 2
                      Skolems =
                      No-eqs = True
                      Status = Solved {Dead givens = []}
                      Given =
                      Wanted = WC {}
                      Binds = EvBindsVar<a1dD>
                      the instance declaration }
  implication evbinds = {[W] $dShow_a1dC
                           = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt}
  implication tvcs = {}
solveImplication {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = False
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2] (CNonCanonical)}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
  Inerts {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                    a_a1ds[ssk:1] (CDictCan)
          Given instances = [G] df_a1e2 {0}:: forall b.
                                              Show b =>
                                              ShowPairlike a_a1ds[ssk:1] b
          Unsolved goals = 0}
         Inert fsks = []
solveWanteds {
  Level = 2
  WC {wc_simple =
        [WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2] (CNonCanonical)}
solveSimpleWanteds {
  {[WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2] (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] $dShow_a1dI {0}:: Show
                                       a_a1dH[tau:2] (CNonCanonical)
  inerts = {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                      a_a1ds[ssk:1] (CDictCan)
            Given instances = [G] df_a1e2 {0}:: forall b.
                                                Show b =>
                                                ShowPairlike a_a1ds[ssk:1] b
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         a_a1dH[tau:2] (CNonCanonical)
canEvNC:cls Show [a_a1dH[tau:2]]
flatten_args { a_a1dH[tau:2]
Following filled tyvar a_a1dH[tau:2] = Pair a_a1ds[ssk:1] Int
Unfilled tyvar a_a1ds[ssk:1]
flatten } Pair a_a1ds[ssk:1] Int
canClass
  [WD] $dShow_a1dI {0}:: Show a_a1dH[tau:2]
  Show (Pair a_a1ds[ssk:1] Int)
  ContinueWith [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan)
doTopReact
  [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)
matchClassInst pred = Show (Pair a_a1ds[ssk:1] Int) {
matchInstEnv
  goal: Show [Pair a_a1ds[ssk:1] Int]
  matches: []
  unify: []
matchClass not matching Show (Pair a_a1ds[ssk:1] Int)
} matchClassInst global result NoInstance
try_fundeps
  [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [WD] $dShow_a1dI {0}:: Show
                                                            (Pair a_a1ds[ssk:1] Int) (CDictCan)
addInertCan }
Step 4[l:2,d:0] Kept as inert:
    [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
End solver pipeline (kept as inert) }
  final_item = [WD] $dShow_a1dI {0}:: Show
                                        (Pair a_a1ds[ssk:1] Int) (CDictCan)
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {[WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan)}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {wc_simple =
                   [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
solveWanteds }
  final wc = WC {wc_simple =
                   [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
  current evbinds  = {}
getNoGivenEqs
  No given equalities
  Skols: []
  Inerts: {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                     a_a1ds[ssk:1] (CDictCan)
           Given instances = [G] df_a1e2 {0}:: forall b.
                                               Show b =>
                                               ShowPairlike a_a1ds[ssk:1] b
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done:
  {[WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
floatEqualities
  Skols = []
  Extended skols = {$dShow_a1dI}
  Simples = {[WD] $dShow_a1dI {0}:: Show
                                      (Pair a_a1ds[ssk:1] Int) (CDictCan)}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {wc_simple =
        [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
setImplicationStatus(not-all-solved) {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {}
  tcvs: {}
  ev_binds: []
  live_ev_binds: []
setImplicationStatus(not-all-solved) }
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Just Implic {
                      TcLevel = 2
                      Skolems =
                      No-eqs = True
                      Status = Unsolved
                      Given =
                      Wanted =
                        WC {wc_simple =
                              [WD] $dShow_a1dI {0}:: Show
                                                       (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
                      Binds = EvBindsVar<a1dJ>
                      the instance declaration }
  implication evbinds = {}
  implication tvcs = {}
solveImplication {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = False
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dP {0}:: Show
                                     (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
    Binds = EvBindsVar<a1dQ>
    the instance declaration }
  Inerts {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                    a_a1ds[ssk:1] (CDictCan)
          Given instances = [G] df_a1e2 {0}:: forall b.
                                              Show b =>
                                              ShowPairlike a_a1ds[ssk:1] b
          Unsolved goals = 0}
         Inert fsks = []
solveWanteds {
  Level = 2
  WC {wc_simple =
        [WD] $dShow_a1dP {0}:: Show
                                 (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
solveSimpleWanteds {
  {[WD] $dShow_a1dP {0}:: Show
                            (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] $dShow_a1dP {0}:: Show
                                       (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)
  inerts = {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                      a_a1ds[ssk:1] (CDictCan)
            Given instances = [G] df_a1e2 {0}:: forall b.
                                                Show b =>
                                                ShowPairlike a_a1ds[ssk:1] b
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShow_a1dP {0}:: Show
                                         (PairWithInt a_a1ds[ssk:1]) (CNonCanonical)
canEvNC:cls Show [PairWithInt a_a1ds[ssk:1]]
flatten_args { PairWithInt a_a1ds[ssk:1]
Unfilled tyvar a_a1ds[ssk:1]
flatten } PairWithInt a_a1ds[ssk:1]
canClass
  [WD] $dShow_a1dP {0}:: Show (PairWithInt a_a1ds[ssk:1])
  Show (PairWithInt a_a1ds[ssk:1])
  ContinueWith [WD] $dShow_a1dP {0}:: Show
                                        (PairWithInt a_a1ds[ssk:1])
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] $dShow_a1dP {0}:: Show
                                         (PairWithInt a_a1ds[ssk:1]) (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] $dShow_a1dP {0}:: Show
                                         (PairWithInt a_a1ds[ssk:1]) (CDictCan)
doTopReact
  [WD] $dShow_a1dP {0}:: Show (PairWithInt a_a1ds[ssk:1]) (CDictCan)
matchClassInst pred = Show (PairWithInt a_a1ds[ssk:1]) {
matchInstEnv
  goal: Show [PairWithInt a_a1ds[ssk:1]]
  matches: [(instance ShowFst a => Show (PairWithInt a)
               -- Defined at test-nested.hs:19:10,
             [Just a_a1ds[ssk:1]])]
  unify: []
matchClass success
  dict Show (PairWithInt a_a1ds[ssk:1])
  witness Foo.$fShowPairWithInt forall a.
                                ShowFst a =>
                                Show (PairWithInt a)
match_one
  Foo.$fShowPairWithInt
  [Just a_a1ds[ssk:1]]
match_one 2
  Foo.$fShowPairWithInt
  [a_a1ds[ssk:1]]
  [ShowFst a_a1ds[ssk:1]]
} matchClassInst global result
  OneInst [ShowFst a_a1ds[ssk:1]]
          top-level instance [safe]
updSolvedSetTcs:
  [WD] $dShow_a1dP {0}:: Show (PairWithInt a_a1ds[ssk:1])
doTopReact/found instance for
  [WD] $dShow_a1dP {0}:: Show (PairWithInt a_a1ds[ssk:1])
newWantedEvVar/cache hit
  [G] $dShowFst_a1dt {0}:: ShowFst a_a1ds[ssk:1]
addTcEvBind
  a1dQ
  [W] $dShow_a1dP
    = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt
end stage top-level reactions }
Step 5[l:2,d:0] Dict/Top (solved wanted):
    [WD] $dShow_a1dP {0}:: Show (PairWithInt a_a1ds[ssk:1])
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {[W] $dShow_a1dP
                        = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt}
getNoGivenEqs
  No given equalities
  Skols: []
  Inerts: {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                     a_a1ds[ssk:1] (CDictCan)
           Given instances = [G] df_a1e2 {0}:: forall b.
                                               Show b =>
                                               ShowPairlike a_a1ds[ssk:1] b
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = []
  Extended skols = {$dShow_a1dP}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted = WC {}
    Binds = EvBindsVar<a1dQ>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {$dShowFst_a1dt, Foo.$fShowPairWithInt}
  tcvs: {}
  ev_binds: [a1dP :-> [W] $dShow_a1dP
                        = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt]
  live_ev_binds: [a1dP :-> [W] $dShow_a1dP
                             = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt]
setImplicationStatus(all-solved) }
  discard: False
  new_implic: Implic {
                TcLevel = 2
                Skolems =
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given =
                Wanted = WC {}
                Binds = EvBindsVar<a1dQ>
                the instance declaration }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Just Implic {
                      TcLevel = 2
                      Skolems =
                      No-eqs = True
                      Status = Solved {Dead givens = []}
                      Given =
                      Wanted = WC {}
                      Binds = EvBindsVar<a1dQ>
                      the instance declaration }
  implication evbinds = {[W] $dShow_a1dP
                           = Foo.$fShowPairWithInt @ a_a1ds[ssk:1] $dShowFst_a1dt}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {}
  unsolved_implics = {Just Implic {
                             TcLevel = 2
                             Skolems =
                             No-eqs = True
                             Status = Solved {Dead givens = []}
                             Given =
                             Wanted = WC {}
                             Binds = EvBindsVar<a1dD>
                             the instance declaration },
                      Just Implic {
                             TcLevel = 2
                             Skolems =
                             No-eqs = True
                             Status = Unsolved
                             Given =
                             Wanted =
                               WC {wc_simple =
                                     [WD] $dShow_a1dI {0}:: Show
                                                              (Pair
                                                                 a_a1ds[ssk:1] Int) (CDictCan(psc))}
                             Binds = EvBindsVar<a1dJ>
                             the instance declaration },
                      Just Implic {
                             TcLevel = 2
                             Skolems =
                             No-eqs = True
                             Status = Solved {Dead givens = []}
                             Given =
                             Wanted = WC {}
                             Binds = EvBindsVar<a1dQ>
                             the instance declaration }}
addTcEvBind
  a1du
  [G] df_a1e3
    = \ (@ b_a1bb) (v_B1 :: Show b_a1bb) ->
        Foo.$p1ShowPairlike
          @ a_a1ds[ssk:1] @ b_a1bb (df_a1e2 @ b_a1bb v_B1)
solveSimpleGivens {
  [[G] df_a1e3 {0}:: forall b (c :: * -> *).
                     (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                     Show (c b) (CNonCanonical)]
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [G] df_a1e3 {0}:: forall b (c :: * -> *).
                                (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                                Show (c b) (CNonCanonical)
  inerts = {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                      a_a1ds[ssk:1] (CDictCan)
            Given instances = [G] df_a1e2 {0}:: forall b.
                                                Show b =>
                                                ShowPairlike a_a1ds[ssk:1] b
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] df_a1e3 {0}:: forall b (c :: * -> *).
                                  (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                                  Show (c b) (CNonCanonical)
canEvNC:forall
  forall (c :: * -> *). (c ~ Pair a_a1ds[ssk:1]) => Show (c b_a1bb)
flatten {
  FM_SubstOnly forall b (c :: * -> *).
               (Show b, c ~ Pair a_a1ds[ssk:1]) =>
               Show (c b)
Unfilled tyvar b_a1bb
Unfilled tyvar c_a1be
Unfilled tyvar a_a1ds[ssk:1]
Unfilled tyvar c_a1be
Unfilled tyvar b_a1bb
flatten }
  forall b (c :: * -> *).
  (Show b, c ~ Pair a_a1ds[ssk:1]) =>
  Show (c b)
end stage canonicalization }
Step 6[l:1,d:0] Given forall-constraint:
    [G] df_a1e3 {0}:: forall b (c :: * -> *).
                      (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                      Show (c b)
End solver pipeline (discharged) }
End solveSimpleGivens }
simpl_loop iteration=0 (no new given superclasses = False, 0 simples to solve)
simpl_loop: wc =
  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = True
          Status = Solved {Dead givens = []}
          Given =
          Wanted = WC {}
          Binds = EvBindsVar<a1dD>
          the instance declaration }
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = True
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1dI {0}:: Show
                                           (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
          Binds = EvBindsVar<a1dJ>
          the instance declaration }
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = True
          Status = Solved {Dead givens = []}
          Given =
          Wanted = WC {}
          Binds = EvBindsVar<a1dQ>
          the instance declaration }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
  Inerts {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                    a_a1ds[ssk:1] (CDictCan)
          Given instances = [G] df_a1e3 {0}:: forall b (c :: * -> *).
                                              (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                                              Show (c b)
                            [G] df_a1e2 {0}:: forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
          Unsolved goals = 0}
         Inert fsks = []
solveWanteds {
  Level = 2
  WC {wc_simple =
        [WD] $dShow_a1dI {0}:: Show
                                 (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
solveSimpleWanteds {
  {[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] $dShow_a1dI {0}:: Show
                                       (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
  inerts = {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                      a_a1ds[ssk:1] (CDictCan)
            Given instances = [G] df_a1e3 {0}:: forall b (c :: * -> *).
                                                (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                                                Show (c b)
                              [G] df_a1e2 {0}:: forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
flatten_args { Pair a_a1ds[ssk:1] Int
Unfilled tyvar a_a1ds[ssk:1]
flatten } Pair a_a1ds[ssk:1] Int
canClass
  [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
  Show (Pair a_a1ds[ssk:1] Int)
  ContinueWith [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
doTopReact
  [WD] $dShow_a1dI {0}:: Show
                           (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
matchClassInst pred = Show (Pair a_a1ds[ssk:1] Int) {
matchInstEnv
  goal: Show [Pair a_a1ds[ssk:1] Int]
  matches: []
  unify: []
matchClass not matching Show (Pair a_a1ds[ssk:1] Int)
} matchClassInst global result NoInstance
try_fundeps
  [WD] $dShow_a1dI {0}:: Show
                           (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [WD] $dShow_a1dI {0}:: Show
                                                            (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
addInertCan }
Step 7[l:2,d:0] Kept as inert:
    [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
End solver pipeline (kept as inert) }
  final_item = [WD] $dShow_a1dI {0}:: Show
                                        (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {[WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {wc_simple =
                   [WD] $dShow_a1dI {0}:: Show
                                            (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
simpl_loop iteration=0 (no new given superclasses = True, 1 simples to solve)
simpl_loop: wc =
  WC {wc_simple =
        [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
solveSimpleWanteds {
  {[WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] $dShow_a1dI {0}:: Show
                                       (Pair a_a1ds[ssk:1] Int) (CDictCan)
  inerts = {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                      a_a1ds[ssk:1] (CDictCan)
            Given instances = [G] df_a1e3 {0}:: forall b (c :: * -> *).
                                                (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                                                Show (c b)
                              [G] df_a1e2 {0}:: forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan)
flatten_args { Pair a_a1ds[ssk:1] Int
Unfilled tyvar a_a1ds[ssk:1]
flatten } Pair a_a1ds[ssk:1] Int
canClass
  [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
  Show (Pair a_a1ds[ssk:1] Int)
  ContinueWith [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [WD] $dShow_a1dI {0}:: Show
                                         (Pair a_a1ds[ssk:1] Int) (CDictCan)
doTopReact
  [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)
matchClassInst pred = Show (Pair a_a1ds[ssk:1] Int) {
matchInstEnv
  goal: Show [Pair a_a1ds[ssk:1] Int]
  matches: []
  unify: []
matchClass not matching Show (Pair a_a1ds[ssk:1] Int)
} matchClassInst global result NoInstance
try_fundeps
  [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [WD] $dShow_a1dI {0}:: Show
                                                            (Pair a_a1ds[ssk:1] Int) (CDictCan)
addInertCan }
Step 8[l:2,d:0] Kept as inert:
    [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int)
End solver pipeline (kept as inert) }
  final_item = [WD] $dShow_a1dI {0}:: Show
                                        (Pair a_a1ds[ssk:1] Int) (CDictCan)
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {[WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan)}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {wc_simple =
                   [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
solveWanteds }
  final wc = WC {wc_simple =
                   [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
  current evbinds  = {}
getNoGivenEqs
  No given equalities
  Skols: []
  Inerts: {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                     a_a1ds[ssk:1] (CDictCan)
           Given instances = [G] df_a1e3 {0}:: forall b (c :: * -> *).
                                               (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                                               Show (c b)
                             [G] df_a1e2 {0}:: forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done:
  {[WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
floatEqualities
  Skols = []
  Extended skols = {$dShow_a1dI}
  Simples = {[WD] $dShow_a1dI {0}:: Show
                                      (Pair a_a1ds[ssk:1] Int) (CDictCan)}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {wc_simple =
        [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
setImplicationStatus(not-all-solved) {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show (Pair a_a1ds[ssk:1] Int) (CDictCan)}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {}
  tcvs: {}
  ev_binds: []
  live_ev_binds: []
setImplicationStatus(not-all-solved) }
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Just Implic {
                      TcLevel = 2
                      Skolems =
                      No-eqs = True
                      Status = Unsolved
                      Given =
                      Wanted =
                        WC {wc_simple =
                              [WD] $dShow_a1dI {0}:: Show
                                                       (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
                      Binds = EvBindsVar<a1dJ>
                      the instance declaration }
  implication evbinds = {}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {}
  unsolved_implics = {Just Implic {
                             TcLevel = 2
                             Skolems =
                             No-eqs = True
                             Status = Solved {Dead givens = []}
                             Given =
                             Wanted = WC {}
                             Binds = EvBindsVar<a1dD>
                             the instance declaration },
                      Just Implic {
                             TcLevel = 2
                             Skolems =
                             No-eqs = True
                             Status = Unsolved
                             Given =
                             Wanted =
                               WC {wc_simple =
                                     [WD] $dShow_a1dI {0}:: Show
                                                              (Pair
                                                                 a_a1ds[ssk:1] Int) (CDictCan(psc))}
                             Binds = EvBindsVar<a1dJ>
                             the instance declaration },
                      Just Implic {
                             TcLevel = 2
                             Skolems =
                             No-eqs = True
                             Status = Solved {Dead givens = []}
                             Given =
                             Wanted = WC {}
                             Binds = EvBindsVar<a1dQ>
                             the instance declaration }}
solveWanteds }
  final wc = WC {wc_impl =
                   Implic {
                     TcLevel = 2
                     Skolems =
                     No-eqs = True
                     Status = Solved {Dead givens = []}
                     Given =
                     Wanted = WC {}
                     Binds = EvBindsVar<a1dD>
                     the instance declaration }
                   Implic {
                     TcLevel = 2
                     Skolems =
                     No-eqs = True
                     Status = Unsolved
                     Given =
                     Wanted =
                       WC {wc_simple =
                             [WD] $dShow_a1dI {0}:: Show
                                                      (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
                     Binds = EvBindsVar<a1dJ>
                     the instance declaration }
                   Implic {
                     TcLevel = 2
                     Skolems =
                     No-eqs = True
                     Status = Solved {Dead givens = []}
                     Given =
                     Wanted = WC {}
                     Binds = EvBindsVar<a1dQ>
                     the instance declaration }}
  current evbinds  = {[G] df_a1e2
                        = Foo.$p1ShowFst @ a_a1ds[ssk:1] $dShowFst_a1dt,
                      [G] df_a1e3
                        = \ (@ b_a1bb) (v_B1 :: Show b_a1bb) ->
                            Foo.$p1ShowPairlike
                              @ a_a1ds[ssk:1] @ b_a1bb (df_a1e2 @ b_a1bb v_B1)}
getNoGivenEqs
  No given equalities
  Skols: [a_a1ds[ssk:1]]
  Inerts: {Dictionaries = [G] $dShowFst_a1dt {0}:: ShowFst
                                                     a_a1ds[ssk:1] (CDictCan)
           Given instances = [G] df_a1e3 {0}:: forall b (c :: * -> *).
                                               (Show b, c ~ Pair a_a1ds[ssk:1]) =>
                                               Show (c b)
                             [G] df_a1e2 {0}:: forall b. Show b => ShowPairlike a_a1ds[ssk:1] b
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = [a_a1ds[ssk:1]]
  Extended skols = {a_a1ds[ssk:1], $dShowFst_a1dt, df_a1e2, df_a1e3}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = True
          Status = Solved {Dead givens = []}
          Given =
          Wanted = WC {}
          Binds = EvBindsVar<a1dD>
          the instance declaration }
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = True
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1dI {0}:: Show
                                           (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
          Binds = EvBindsVar<a1dJ>
          the instance declaration }
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = True
          Status = Solved {Dead givens = []}
          Given =
          Wanted = WC {}
          Binds = EvBindsVar<a1dQ>
          the instance declaration }}
setImplicationStatus(not-all-solved) {
  Implic {
    TcLevel = 1
    Skolems = a_a1ds[ssk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Solved {Dead givens = []}
              Given =
              Wanted = WC {}
              Binds = EvBindsVar<a1dD>
              the instance declaration }
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dI {0}:: Show
                                               (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
              Binds = EvBindsVar<a1dJ>
              the instance declaration }
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Solved {Dead givens = []}
              Given =
              Wanted = WC {}
              Binds = EvBindsVar<a1dQ>
              the instance declaration }}
    Binds = EvBindsVar<a1du>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {$dShowFst_a1dt, Foo.$fShowPairWithInt}
  tcvs: {}
  ev_binds: [a1e2 :-> [G] df_a1e2
                        = Foo.$p1ShowFst @ a_a1ds[ssk:1] $dShowFst_a1dt,
             a1e3 :-> [G] df_a1e3
                        = \ (@ b_a1bb) (v_B1 :: Show b_a1bb) ->
                            Foo.$p1ShowPairlike
                              @ a_a1ds[ssk:1] @ b_a1bb (df_a1e2 @ b_a1bb v_B1)]
  live_ev_binds: []
setImplicationStatus(not-all-solved) }
  Implic {
    TcLevel = 1
    Skolems = a_a1ds[ssk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dI {0}:: Show
                                               (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
              Binds = EvBindsVar<a1dJ>
              the instance declaration }}
    Binds = EvBindsVar<a1du>
    the instance declaration }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Just Implic {
                      TcLevel = 1
                      Skolems = a_a1ds[ssk:1]
                      No-eqs = True
                      Status = Unsolved
                      Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
                      Wanted =
                        WC {wc_impl =
                              Implic {
                                TcLevel = 2
                                Skolems =
                                No-eqs = True
                                Status = Unsolved
                                Given =
                                Wanted =
                                  WC {wc_simple =
                                        [WD] $dShow_a1dI {0}:: Show
                                                                 (Pair
                                                                    a_a1ds[ssk:1]
                                                                    Int) (CDictCan(psc))}
                                Binds = EvBindsVar<a1dJ>
                                the instance declaration }}
                      Binds = EvBindsVar<a1du>
                      the instance declaration }
  implication evbinds = {}
  implication tvcs = {}
solveImplication {
  Implic {
    TcLevel = 1
    Skolems = a_a1dS[ssk:1] b_a1dT[ssk:1]
    No-eqs = False
    Status = Unsolved
    Given = $dShow_a1dU :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] df_a1dW {0}:: forall (c :: * -> *).
                                         (c ~ Pair a_a1dS[ssk:1]) =>
                                         Show (c b_a1dT[ssk:1]) (CNonCanonical)}
              Binds = EvBindsVar<a1dX>
              the instance declaration }}
    Binds = EvBindsVar<a1dV>
    the instance declaration }
  Inerts {Unsolved goals = 0}
         Inert fsks = []
solveSimpleGivens {
  [[G] $dShow_a1dU {0}:: Show
                           (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CNonCanonical)]
----------------------------- 
Start solver pipeline {
  tclevel = 1
  work item = [G] $dShow_a1dU {0}:: Show
                                      (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CNonCanonical)
  inerts = {Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] $dShow_a1dU {0}:: Show
                                        (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CNonCanonical)
canEvNC:cls Show [Pair a_a1dS[ssk:1] b_a1dT[ssk:1]]
Emitting fresh work
flatten_args { Pair a_a1dS[ssk:1] b_a1dT[ssk:1]
Unfilled tyvar a_a1dS[ssk:1]
Unfilled tyvar b_a1dT[ssk:1]
flatten } Pair a_a1dS[ssk:1] b_a1dT[ssk:1]
canClass
  [G] $dShow_a1dU {0}:: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
  Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
  ContinueWith [G] $dShow_a1dU {0}:: Show
                                       (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [G] $dShow_a1dU {0}:: Show
                                        (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [G] $dShow_a1dU {0}:: Show
                                        (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
doTopReact
  [G] $dShow_a1dU {0}:: Show
                          (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
try_fundeps
  [G] $dShow_a1dU {0}:: Show
                          (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [G] $dShow_a1dU {0}:: Show
                                                           (Pair
                                                              a_a1dS[ssk:1]
                                                              b_a1dT[ssk:1]) (CDictCan)
addInertCan }
Step 9[l:1,d:0] Kept as inert:
    [G] $dShow_a1dU {0}:: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
End solver pipeline (kept as inert) }
  final_item = [G] $dShow_a1dU {0}:: Show
                                       (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
End solveSimpleGivens }
solveWanteds {
  Level = 1
  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = False
          Status = Unsolved
          Given =
          Wanted =
            WC {wc_simple =
                  [WD] df_a1dW {0}:: forall (c :: * -> *).
                                     (c ~ Pair a_a1dS[ssk:1]) =>
                                     Show (c b_a1dT[ssk:1]) (CNonCanonical)}
          Binds = EvBindsVar<a1dX>
          the instance declaration }}
solveSimpleWanteds { {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = False
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] df_a1dW {0}:: forall (c :: * -> *).
                               (c ~ Pair a_a1dS[ssk:1]) =>
                               Show (c b_a1dT[ssk:1]) (CNonCanonical)}
    Binds = EvBindsVar<a1dX>
    the instance declaration }
  Inerts {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                 (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
          Unsolved goals = 0}
         Inert fsks = []
solveWanteds {
  Level = 2
  WC {wc_simple =
        [WD] df_a1dW {0}:: forall (c :: * -> *).
                           (c ~ Pair a_a1dS[ssk:1]) =>
                           Show (c b_a1dT[ssk:1]) (CNonCanonical)}
solveSimpleWanteds {
  {[WD] df_a1dW {0}:: forall (c :: * -> *).
                      (c ~ Pair a_a1dS[ssk:1]) =>
                      Show (c b_a1dT[ssk:1]) (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 2
  work item = [WD] df_a1dW {0}:: forall (c :: * -> *).
                                 (c ~ Pair a_a1dS[ssk:1]) =>
                                 Show (c b_a1dT[ssk:1]) (CNonCanonical)
  inerts = {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                   (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] df_a1dW {0}:: forall (c :: * -> *).
                                   (c ~ Pair a_a1dS[ssk:1]) =>
                                   Show (c b_a1dT[ssk:1]) (CNonCanonical)
canEvNC:forall Show (c_a1be b_a1dT[ssk:1])
flatten {
  FM_SubstOnly forall (c :: * -> *).
               (c ~ Pair a_a1dS[ssk:1]) =>
               Show (c b_a1dT[ssk:1])
Unfilled tyvar c_a1be
Unfilled tyvar a_a1dS[ssk:1]
Unfilled tyvar c_a1be
Unfilled tyvar b_a1dT[ssk:1]
flatten }
  forall (c :: * -> *).
  (c ~ Pair a_a1dS[ssk:1]) =>
  Show (c b_a1dT[ssk:1])
Emitting new wanted
  $dShow_a1e6 :: Show (c_a1e4[sk:3] b_a1dT[ssk:1])
  arising from the superclasses of an instance declaration
  at test-nested.hs:13:10-44
newTcEvBinds unique = a1e7
addTcEvBind
  a1dX
  [W] df_a1dW
    = \ (@ (c_a1e4[sk:3] :: * -> *))
        ($d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]) ->
        EvBindsVar<a1e7>
        $dShow_a1e6
end stage canonicalization }
Step 10[l:2,d:0] Wanted forall-constraint:
    [WD] df_a1dW {0}:: forall (c :: * -> *).
                       (c ~ Pair a_a1dS[ssk:1]) =>
                       Show (c b_a1dT[ssk:1])
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {Implic {
               TcLevel = 3
               Skolems = (c_a1e4[sk:3] :: * -> *)
               No-eqs = False
               Status = Unsolved
               Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
               Wanted =
                 WC {wc_simple =
                       [WD] $dShow_a1e6 {0}:: Show
                                                (c_a1e4[sk:3] b_a1dT[ssk:1]) (CNonCanonical)}
               Binds = EvBindsVar<a1e7>
               a quantified context }}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {wc_impl =
                   Implic {
                     TcLevel = 3
                     Skolems = (c_a1e4[sk:3] :: * -> *)
                     No-eqs = False
                     Status = Unsolved
                     Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
                     Wanted =
                       WC {wc_simple =
                             [WD] $dShow_a1e6 {0}:: Show
                                                      (c_a1e4[sk:3] b_a1dT[ssk:1]) (CNonCanonical)}
                     Binds = EvBindsVar<a1e7>
                     a quantified context }}
solveNestedImplications starting {
solveImplication {
  Implic {
    TcLevel = 3
    Skolems = (c_a1e4[sk:3] :: * -> *)
    No-eqs = False
    Status = Unsolved
    Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1e6 {0}:: Show
                                     (c_a1e4[sk:3] b_a1dT[ssk:1]) (CNonCanonical)}
    Binds = EvBindsVar<a1e7>
    a quantified context }
  Inerts {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                 (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
          Unsolved goals = 0}
         Inert fsks = []
solveSimpleGivens {
  [[G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                      ~ Pair a_a1dS[ssk:1] (CNonCanonical)]
----------------------------- 
Start solver pipeline {
  tclevel = 3
  work item = [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                 ~ Pair a_a1dS[ssk:1] (CNonCanonical)
  inerts = {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                   (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                   ~ Pair a_a1dS[ssk:1] (CNonCanonical)
canEvNC:cls ~ [* -> *, c_a1e4[sk:3], Pair a_a1dS[ssk:1]]
addTcEvBind
  a1e7
  [G] co_a1e8
    = GHC.Types.eq_sel
        @ (* -> *) @ c_a1e4[sk:3] @ (Pair a_a1dS[ssk:1]) $d~_a1e5
Emitting fresh work
  [G] co_a1e8 {0}:: c_a1e4[sk:3]
                    GHC.Prim.~# Pair a_a1dS[ssk:1] (CNonCanonical)
flatten_args {
  * -> *
  c_a1e4[sk:3]
  Pair a_a1dS[ssk:1]
Unfilled tyvar c_a1e4[sk:3]
Unfilled tyvar a_a1dS[ssk:1]
flatten }
  * -> *
  c_a1e4[sk:3]
  Pair a_a1dS[ssk:1]
canClass
  [G] $d~_a1e5 {0}:: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
  c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
  ContinueWith [G] $d~_a1e5 {0}:: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                   ~ Pair a_a1dS[ssk:1] (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                   ~ Pair a_a1dS[ssk:1] (CDictCan)
doTopReact
  [G] $d~_a1e5 {0}:: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1] (CDictCan)
try_fundeps
  [G] $d~_a1e5 {0}:: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1] (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                                      ~ Pair a_a1dS[ssk:1] (CDictCan)
addInertCan }
Step 11[l:3,d:0] Kept as inert:
    [G] $d~_a1e5 {0}:: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
End solver pipeline (kept as inert) }
  final_item = [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                  ~ Pair a_a1dS[ssk:1] (CDictCan)
----------------------------- 
Start solver pipeline {
  tclevel = 3
  work item = [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                GHC.Prim.~# Pair a_a1dS[ssk:1] (CNonCanonical)
  inerts = {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                   (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
                           [G] $d~_a1e5 {0}:: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1] (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                  GHC.Prim.~# Pair a_a1dS[ssk:1] (CNonCanonical)
canEvNC:eq
  c_a1e4[sk:3]
  Pair a_a1dS[ssk:1]
can_eq_nc
  False
  [G] co_a1e8 {0}:: c_a1e4[sk:3] GHC.Prim.~# Pair a_a1dS[ssk:1]
  nominal equality
  c_a1e4[sk:3]
  c_a1e4[sk:3]
  Pair a_a1dS[ssk:1]
  Pair a_a1dS[ssk:1]
flatten { FM_FlattenAll c_a1e4[sk:3]
Unfilled tyvar c_a1e4[sk:3]
flatten } c_a1e4[sk:3]
flatten { FM_FlattenAll Pair a_a1dS[ssk:1]
Unfilled tyvar a_a1dS[ssk:1]
flatten } Pair a_a1dS[ssk:1]
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                  GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                  GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
doTopReact
  [G] co_a1e8 {0}:: c_a1e4[sk:3]
                    GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                                     GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
Kick out, tv = c_a1e4[sk:3]
  n-kicked = 1
  kicked_out = WL {Eqs = [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                            ~ Pair a_a1dS[ssk:1] (CDictCan)}
  Residual inerts = {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                            (Pair
                                                               a_a1dS[ssk:1]
                                                               b_a1dT[ssk:1]) (CDictCan)
                     Unsolved goals = 0}
addInertCan }
Step 12[l:3,d:0] Kept as inert:
    [G] co_a1e8 {0}:: c_a1e4[sk:3] GHC.Prim.~# Pair a_a1dS[ssk:1]
End solver pipeline (kept as inert) }
  final_item = [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                 GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
----------------------------- 
Start solver pipeline {
  tclevel = 3
  work item = [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                 ~ Pair a_a1dS[ssk:1] (CDictCan)
  inerts = {Equalities: [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                          GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
            Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                   (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [G] $d~_a1e5 {0}:: c_a1e4[sk:3]
                                   ~ Pair a_a1dS[ssk:1] (CDictCan)
flatten_args {
  * -> *
  c_a1e4[sk:3]
  Pair a_a1dS[ssk:1]
Unfilled tyvar c_a1e4[sk:3]
Following inert tyvar
  FM_FlattenAll c_a1e4[sk:3] = Pair a_a1dS[ssk:1]
  [G] co_a1e8 {0}:: c_a1e4[sk:3] GHC.Prim.~# Pair a_a1dS[ssk:1]
Unfilled tyvar a_a1dS[ssk:1]
Unfilled tyvar a_a1dS[ssk:1]
flatten }
  * -> *
  Pair a_a1dS[ssk:1]
  Pair a_a1dS[ssk:1]
addTcEvBind
  a1e7
  [G] $d~_a1e9
    = $d~_a1e5
      `cast` (Sub (Sym ((~)
                          <* -> *>_N (Sym co_a1e8) <Pair a_a1dS[ssk:1]>_N)_N)
              :: (c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1])
                 ~R# (Pair a_a1dS[ssk:1] ~ Pair a_a1dS[ssk:1]))
canClass
  [G] $d~_a1e5 {0}:: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
  Pair a_a1dS[ssk:1] ~ Pair a_a1dS[ssk:1]
  ContinueWith [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                                  ~ Pair a_a1dS[ssk:1]
end stage canonicalization }
runStage interact with inerts {
  workitem   =  [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                                   ~ Pair a_a1dS[ssk:1] (CDictCan)
end stage interact with inerts }
runStage top-level reactions {
  workitem   =  [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                                   ~ Pair a_a1dS[ssk:1] (CDictCan)
doTopReact
  [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                     ~ Pair a_a1dS[ssk:1] (CDictCan)
try_fundeps
  [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                     ~ Pair a_a1dS[ssk:1] (CDictCan)
end stage top-level reactions }
insertInertCan {
  Trying to insert new inert item: [G] $d~_a1e9 {0}:: Pair
                                                        a_a1dS[ssk:1]
                                                      ~ Pair a_a1dS[ssk:1] (CDictCan)
addInertCan }
Step 13[l:3,d:0] Kept as inert:
    [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1] ~ Pair a_a1dS[ssk:1]
End solver pipeline (kept as inert) }
  final_item = [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                                  ~ Pair a_a1dS[ssk:1] (CDictCan)
End solveSimpleGivens }
solveWanteds {
  Level = 3
  WC {wc_simple =
        [WD] $dShow_a1e6 {0}:: Show
                                 (c_a1e4[sk:3] b_a1dT[ssk:1]) (CNonCanonical)}
solveSimpleWanteds {
  {[WD] $dShow_a1e6 {0}:: Show
                            (c_a1e4[sk:3] b_a1dT[ssk:1]) (CNonCanonical)}
----------------------------- 
Start solver pipeline {
  tclevel = 3
  work item = [WD] $dShow_a1e6 {0}:: Show
                                       (c_a1e4[sk:3] b_a1dT[ssk:1]) (CNonCanonical)
  inerts = {Equalities: [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                          GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
            Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                   (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
                           [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                                              ~ Pair a_a1dS[ssk:1] (CDictCan)
            Unsolved goals = 0}
           Inert fsks = []
  rest of worklist = WL {}
runStage canonicalization {
  workitem   =  [WD] $dShow_a1e6 {0}:: Show
                                         (c_a1e4[sk:3] b_a1dT[ssk:1]) (CNonCanonical)
canEvNC:cls Show [c_a1e4[sk:3] b_a1dT[ssk:1]]
flatten_args { c_a1e4[sk:3] b_a1dT[ssk:1]
Unfilled tyvar c_a1e4[sk:3]
Following inert tyvar
  FM_FlattenAll c_a1e4[sk:3] = Pair a_a1dS[ssk:1]
  [G] co_a1e8 {0}:: c_a1e4[sk:3] GHC.Prim.~# Pair a_a1dS[ssk:1]
Unfilled tyvar a_a1dS[ssk:1]
Unfilled tyvar b_a1dT[ssk:1]
flatten } Pair a_a1dS[ssk:1] b_a1dT[ssk:1]
newWantedEvVar/cache hit
  [G] $dShow_a1dU {0}:: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
addTcEvBind
  a1e7
  [W] $dShow_a1e6
    = $dShow_a1dU
      `cast` ((Show (Sym co_a1e8 <b_a1dT[ssk:1]>_N))_R
              :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
                 ~R# Show (c_a1e4[sk:3] b_a1dT[ssk:1]))
canClass
  [WD] $dShow_a1e6 {0}:: Show (c_a1e4[sk:3] b_a1dT[ssk:1])
  Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
  Stop(Cached wanted) [WD] $dShow_a1e6 {0}:: Show
                                               (c_a1e4[sk:3] b_a1dT[ssk:1])
end stage canonicalization }
Step 14[l:3,d:0] Cached wanted:
    [WD] $dShow_a1e6 {0}:: Show (c_a1e4[sk:3] b_a1dT[ssk:1])
End solver pipeline (discharged) }
getUnsolvedInerts
   tv eqs = {}
  fun eqs = {}
  others = {}
  implics = {}
Unflattening
  {Funeqs =
   Tv eqs =}
Unflattening 1 {}
Unflattening 2 {}
Unflattening 3 {}
Unflattening done {}
solveSimpleWanteds end }
  iterations = 1
  residual = WC {}
solveWanteds }
  final wc = WC {}
  current evbinds  = {[G] co_a1e8
                        = GHC.Types.eq_sel
                            @ (* -> *) @ c_a1e4[sk:3] @ (Pair a_a1dS[ssk:1]) $d~_a1e5,
                      [G] $d~_a1e9
                        = $d~_a1e5
                          `cast` (Sub (Sym ((~)
                                              <* -> *>_N (Sym co_a1e8) <Pair a_a1dS[ssk:1]>_N)_N)
                                  :: (c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1])
                                     ~R# (Pair a_a1dS[ssk:1] ~ Pair a_a1dS[ssk:1])),
                      [W] $dShow_a1e6
                        = $dShow_a1dU
                          `cast` ((Show (Sym co_a1e8 <b_a1dT[ssk:1]>_N))_R
                                  :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
                                     ~R# Show (c_a1e4[sk:3] b_a1dT[ssk:1]))}
getNoGivenEqs
  No given equalities
  Skols: [c_a1e4[sk:3]]
  Inerts: {Equalities: [G] co_a1e8 {0}:: c_a1e4[sk:3]
                                         GHC.Prim.~# Pair a_a1dS[ssk:1] (CTyEqCan)
           Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                  (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
                          [G] $d~_a1e9 {0}:: Pair a_a1dS[ssk:1]
                                             ~ Pair a_a1dS[ssk:1] (CDictCan)
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = [c_a1e4[sk:3]]
  Extended skols = {c_a1e4[sk:3], $d~_a1e5, $dShow_a1e6, co_a1e8,
                    $d~_a1e9}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 3
    Skolems = (c_a1e4[sk:3] :: * -> *)
    No-eqs = True
    Status = Unsolved
    Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
    Wanted = WC {}
    Binds = EvBindsVar<a1e7>
    a quantified context }
neededEvVars
  old_needs: {}
  seeds3: {$dShow_a1dU, co_a1e8}
  tcvs: {}
  ev_binds: [a1e8 :-> [G] co_a1e8
                        = GHC.Types.eq_sel
                            @ (* -> *) @ c_a1e4[sk:3] @ (Pair a_a1dS[ssk:1]) $d~_a1e5,
             a1e9 :-> [G] $d~_a1e9
                        = $d~_a1e5
                          `cast` (Sub (Sym ((~)
                                              <* -> *>_N (Sym co_a1e8) <Pair a_a1dS[ssk:1]>_N)_N)
                                  :: (c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1])
                                     ~R# (Pair a_a1dS[ssk:1] ~ Pair a_a1dS[ssk:1])),
             a1e6 :-> [W] $dShow_a1e6
                        = $dShow_a1dU
                          `cast` ((Show (Sym co_a1e8 <b_a1dT[ssk:1]>_N))_R
                                  :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
                                     ~R# Show (c_a1e4[sk:3] b_a1dT[ssk:1]))]
  live_ev_binds: [a1e8 :-> [G] co_a1e8
                             = GHC.Types.eq_sel
                                 @ (* -> *) @ c_a1e4[sk:3] @ (Pair a_a1dS[ssk:1]) $d~_a1e5,
                  a1e6 :-> [W] $dShow_a1e6
                             = $dShow_a1dU
                               `cast` ((Show (Sym co_a1e8 <b_a1dT[ssk:1]>_N))_R
                                       :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
                                          ~R# Show (c_a1e4[sk:3] b_a1dT[ssk:1]))]
setImplicationStatus(all-solved) }
  discard: False
  new_implic: Implic {
                TcLevel = 3
                Skolems = (c_a1e4[sk:3] :: * -> *)
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
                Wanted = WC {}
                Binds = EvBindsVar<a1e7>
                a quantified context }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Just Implic {
                      TcLevel = 3
                      Skolems = (c_a1e4[sk:3] :: * -> *)
                      No-eqs = True
                      Status = Solved {Dead givens = []}
                      Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
                      Wanted = WC {}
                      Binds = EvBindsVar<a1e7>
                      a quantified context }
  implication evbinds = {[G] co_a1e8
                           = GHC.Types.eq_sel
                               @ (* -> *) @ c_a1e4[sk:3] @ (Pair a_a1dS[ssk:1]) $d~_a1e5,
                         [W] $dShow_a1e6
                           = $dShow_a1dU
                             `cast` ((Show (Sym co_a1e8 <b_a1dT[ssk:1]>_N))_R
                                     :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
                                        ~R# Show (c_a1e4[sk:3] b_a1dT[ssk:1]))}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {}
  unsolved_implics = {Just Implic {
                             TcLevel = 3
                             Skolems = (c_a1e4[sk:3] :: * -> *)
                             No-eqs = True
                             Status = Solved {Dead givens = []}
                             Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
                             Wanted = WC {}
                             Binds = EvBindsVar<a1e7>
                             a quantified context }}
solveWanteds }
  final wc = WC {wc_impl =
                   Implic {
                     TcLevel = 3
                     Skolems = (c_a1e4[sk:3] :: * -> *)
                     No-eqs = True
                     Status = Solved {Dead givens = []}
                     Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
                     Wanted = WC {}
                     Binds = EvBindsVar<a1e7>
                     a quantified context }}
  current evbinds  = {[W] df_a1dZ = df_a1dW,
                      [W] df_a1dW
                        = \ (@ (c_a1e4[sk:3] :: * -> *))
                            ($d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]) ->
                            EvBindsVar<a1e7>
                            $dShow_a1e6}
getNoGivenEqs
  No given equalities
  Skols: []
  Inerts: {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                  (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = []
  Extended skols = {df_a1dW, df_a1dZ}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {wc_impl =
        Implic {
          TcLevel = 3
          Skolems = (c_a1e4[sk:3] :: * -> *)
          No-eqs = True
          Status = Solved {Dead givens = []}
          Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
          Wanted = WC {}
          Binds = EvBindsVar<a1e7>
          a quantified context }}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 3
              Skolems = (c_a1e4[sk:3] :: * -> *)
              No-eqs = True
              Status = Solved {Dead givens = []}
              Given = $d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]
              Wanted = WC {}
              Binds = EvBindsVar<a1e7>
              a quantified context }}
    Binds = EvBindsVar<a1dX>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {$dShow_a1dU, df_a1dW}
  tcvs: {}
  ev_binds: [a1dZ :-> [W] df_a1dZ = df_a1dW,
             a1dW :-> [W] df_a1dW
                        = \ (@ (c_a1e4[sk:3] :: * -> *))
                            ($d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]) ->
                            EvBindsVar<a1e7>
                            $dShow_a1e6]
  live_ev_binds: [a1dZ :-> [W] df_a1dZ = df_a1dW,
                  a1dW :-> [W] df_a1dW
                             = \ (@ (c_a1e4[sk:3] :: * -> *))
                                 ($d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]) ->
                                 EvBindsVar<a1e7>
                                 $dShow_a1e6]
setImplicationStatus(all-solved) }
  discard: False
  new_implic: Implic {
                TcLevel = 2
                Skolems =
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given =
                Wanted = WC {}
                Binds = EvBindsVar<a1dX>
                the instance declaration }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Just Implic {
                      TcLevel = 2
                      Skolems =
                      No-eqs = True
                      Status = Solved {Dead givens = []}
                      Given =
                      Wanted = WC {}
                      Binds = EvBindsVar<a1dX>
                      the instance declaration }
  implication evbinds = {[W] df_a1dZ = df_a1dW,
                         [W] df_a1dW
                           = \ (@ (c_a1e4[sk:3] :: * -> *))
                               ($d~_a1e5 :: c_a1e4[sk:3] ~ Pair a_a1dS[ssk:1]) ->
                               EvBindsVar<a1e7>
                               $dShow_a1e6}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {}
  unsolved_implics = {Just Implic {
                             TcLevel = 2
                             Skolems =
                             No-eqs = True
                             Status = Solved {Dead givens = []}
                             Given =
                             Wanted = WC {}
                             Binds = EvBindsVar<a1dX>
                             the instance declaration }}
solveWanteds }
  final wc = WC {wc_impl =
                   Implic {
                     TcLevel = 2
                     Skolems =
                     No-eqs = True
                     Status = Solved {Dead givens = []}
                     Given =
                     Wanted = WC {}
                     Binds = EvBindsVar<a1dX>
                     the instance declaration }}
  current evbinds  = {}
getNoGivenEqs
  No given equalities
  Skols: [a_a1dS[ssk:1], b_a1dT[ssk:1]]
  Inerts: {Dictionaries = [G] $dShow_a1dU {0}:: Show
                                                  (Pair a_a1dS[ssk:1] b_a1dT[ssk:1]) (CDictCan)
           Unsolved goals = 0}
  Insols: {}
unflattenGivens []
zonkSimples done: {}
floatEqualities
  Skols = [a_a1dS[ssk:1], b_a1dT[ssk:1]]
  Extended skols = {a_a1dS[ssk:1], b_a1dT[ssk:1], $dShow_a1dU}
  Simples = {}
  Candidate eqs = {}
  Floated eqs = {}
solveImplication 2
  {}
  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems =
          No-eqs = True
          Status = Solved {Dead givens = []}
          Given =
          Wanted = WC {}
          Binds = EvBindsVar<a1dX>
          the instance declaration }}
setImplicationStatus(all-solved) {
  Implic {
    TcLevel = 1
    Skolems = a_a1dS[ssk:1] b_a1dT[ssk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShow_a1dU :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Solved {Dead givens = []}
              Given =
              Wanted = WC {}
              Binds = EvBindsVar<a1dX>
              the instance declaration }}
    Binds = EvBindsVar<a1dV>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {$dShow_a1dU}
  tcvs: {}
  ev_binds: []
  live_ev_binds: []
setImplicationStatus(all-solved) }
  discard: True
  new_implic: Implic {
                TcLevel = 1
                Skolems = a_a1dS[ssk:1] b_a1dT[ssk:1]
                No-eqs = True
                Status = Solved {Dead givens = []}
                Given = $dShow_a1dU :: Show (Pair a_a1dS[ssk:1] b_a1dT[ssk:1])
                Wanted = WC {}
                Binds = EvBindsVar<a1dV>
                the instance declaration }
solveImplication end }
  no_given_eqs = True
  floated_eqs = {}
  res_implic = Nothing
  implication evbinds = {}
  implication tvcs = {}
solveNestedImplications end }
  all floated_eqs = {}
  unsolved_implics = {Just Implic {
                             TcLevel = 1
                             Skolems = a_a1ds[ssk:1]
                             No-eqs = True
                             Status = Unsolved
                             Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
                             Wanted =
                               WC {wc_impl =
                                     Implic {
                                       TcLevel = 2
                                       Skolems =
                                       No-eqs = True
                                       Status = Unsolved
                                       Given =
                                       Wanted =
                                         WC {wc_simple =
                                               [WD] $dShow_a1dI {0}:: Show
                                                                        (Pair
                                                                           a_a1ds[ssk:1]
                                                                           Int) (CDictCan(psc))}
                                       Binds = EvBindsVar<a1dJ>
                                       the instance declaration }}
                             Binds = EvBindsVar<a1du>
                             the instance declaration },
                      Nothing}
solveWanteds }
  final wc = WC {wc_impl =
                   Implic {
                     TcLevel = 1
                     Skolems = a_a1ds[ssk:1]
                     No-eqs = True
                     Status = Unsolved
                     Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
                     Wanted =
                       WC {wc_impl =
                             Implic {
                               TcLevel = 2
                               Skolems =
                               No-eqs = True
                               Status = Unsolved
                               Given =
                               Wanted =
                                 WC {wc_simple =
                                       [WD] $dShow_a1dI {0}:: Show
                                                                (Pair
                                                                   a_a1ds[ssk:1]
                                                                   Int) (CDictCan(psc))}
                               Binds = EvBindsVar<a1dJ>
                               the instance declaration }}
                     Binds = EvBindsVar<a1du>
                     the instance declaration }}
  current evbinds  = {}
zonkSimples done: {}
zonkSimples done: {}
zonkSimples done:
  {[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
applyDefaultingRules {
  wanteds = WC {wc_impl =
                  Implic {
                    TcLevel = 1
                    Skolems = a_a1ds[ssk:1]
                    No-eqs = True
                    Status = Unsolved
                    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
                    Wanted =
                      WC {wc_impl =
                            Implic {
                              TcLevel = 2
                              Skolems =
                              No-eqs = True
                              Status = Unsolved
                              Given =
                              Wanted =
                                WC {wc_simple =
                                      [WD] $dShow_a1dI {0}:: Show
                                                               (Pair
                                                                  a_a1ds[ssk:1]
                                                                  Int) (CDictCan(psc))}
                              Binds = EvBindsVar<a1dJ>
                              the instance declaration }}
                    Binds = EvBindsVar<a1du>
                    the instance declaration }}
  groups  = []
  info    = ([Integer, Double], (False, False))
applyDefaultingRules } []
setImplicationStatus(not-all-solved) {
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
neededEvVars
  old_needs: {}
  seeds3: {}
  tcvs: {}
  ev_binds: []
  live_ev_binds: []
setImplicationStatus(not-all-solved) }
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
setImplicationStatus(not-all-solved) {
  Implic {
    TcLevel = 1
    Skolems = a_a1ds[ssk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dI {0}:: Show
                                               (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
              Binds = EvBindsVar<a1dJ>
              the instance declaration }}
    Binds = EvBindsVar<a1du>
    the instance declaration }
neededEvVars
  old_needs: {$dShowFst_a1dt, Foo.$fShowPairWithInt}
  seeds3: {$dShowFst_a1dt, Foo.$fShowPairWithInt}
  tcvs: {}
  ev_binds: []
  live_ev_binds: []
setImplicationStatus(not-all-solved) }
  Implic {
    TcLevel = 1
    Skolems = a_a1ds[ssk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dI {0}:: Show
                                               (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
              Binds = EvBindsVar<a1dJ>
              the instance declaration }}
    Binds = EvBindsVar<a1du>
    the instance declaration }
Constraint solver steps = 14
unflattenGivens []
End simplifyTop }
newTcEvBinds unique = a1oY
reportUnsolved {
  type errors: TypeError
  expr holes: HoleError
  type holes: HoleError
  scope holes: HoleError
reportUnsolved (before zonking and tidying)
  WC {wc_impl =
        Implic {
          TcLevel = 1
          Skolems = a_a1ds[ssk:1]
          No-eqs = True
          Status = Unsolved
          Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
          Wanted =
            WC {wc_impl =
                  Implic {
                    TcLevel = 2
                    Skolems =
                    No-eqs = True
                    Status = Unsolved
                    Given =
                    Wanted =
                      WC {wc_simple =
                            [WD] $dShow_a1dI {0}:: Show
                                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
                    Binds = EvBindsVar<a1dJ>
                    the instance declaration }}
          Binds = EvBindsVar<a1du>
          the instance declaration }}
zonkSimples done: {}
zonkSimples done: {}
zonkSimples done:
  {[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
reportUnsolved (after zonking):
  Free tyvars:
  Tidy env: ([], [])
  Wanted: WC {wc_impl =
                Implic {
                  TcLevel = 1
                  Skolems = a_a1ds[ssk:1]
                  No-eqs = True
                  Status = Unsolved
                  Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
                  Wanted =
                    WC {wc_impl =
                          Implic {
                            TcLevel = 2
                            Skolems =
                            No-eqs = True
                            Status = Unsolved
                            Given =
                            Wanted =
                              WC {wc_simple =
                                    [WD] $dShow_a1dI {0}:: Show
                                                             (Pair
                                                                a_a1ds[ssk:1] Int) (CDictCan(psc))}
                            Binds = EvBindsVar<a1dJ>
                            the instance declaration }}
                  Binds = EvBindsVar<a1du>
                  the instance declaration }}
reportWanteds
  Simples = {}
  Suppress = False
rw2 []
tryReporters {
  []
  []
tryReporters } []
tryReporters {
  []
  []
tryReporters } []
reportImplic
  Implic {
    TcLevel = 1
    Skolems = a_a1ds[ssk:1]
    No-eqs = True
    Status = Unsolved
    Given = $dShowFst_a1dt :: ShowFst a_a1ds[ssk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = True
              Status = Unsolved
              Given =
              Wanted =
                WC {wc_simple =
                      [WD] $dShow_a1dI {0}:: Show
                                               (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
              Binds = EvBindsVar<a1dJ>
              the instance declaration }}
    Binds = EvBindsVar<a1du>
    the instance declaration }
reportWanteds
  Simples = {}
  Suppress = False
rw2 []
tryReporters {
  []
  []
tryReporters } []
tryReporters {
  []
  []
tryReporters } []
reportImplic
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_simple =
            [WD] $dShow_a1dI {0}:: Show
                                     (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
    Binds = EvBindsVar<a1dJ>
    the instance declaration }
reportWanteds
  Simples = {[WD] $dShow_a1dI {0}:: Show
                                      (Pair a_a1ds[ssk:1] Int) (CDictCan(psc))}
  Suppress = False
rw2
  [[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CNonCanonical)]
tryReporters {
  [[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CNonCanonical)]
  []
tryReporters }
  [[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CNonCanonical)]
tryReporters {
  [[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CNonCanonical)]
  []
tryReporter{ 
  Dicts [[WD] $dShow_a1dI {0}:: Show
                                  (Pair a_a1ds[ssk:1] Int) (CNonCanonical)]
relevantBindings
  [WD] $dShow_a1dI {0}:: Show
                           (Pair a_a1ds[ssk:1] Int) (CNonCanonical)
  arising from a use of ‘show’
  {a_a1ds[ssk:1]}
  x_a1bi :: Pair a_a1ds[ssk:1] Int,
  show_a1dG :: PairWithInt a_a1ds[ssk:1] -> String
relevantBindings 1 x_a1bi :: Pair a_a1ds[ssk:1] Int
relevantBindings 1 show_a1dG :: PairWithInt a_a1ds[ssk:1] -> String
About to maybeReportErr
  Constraint: [[WD] $dShow_a1dI {0}:: Show
                                        (Pair a_a1ds[ssk:1] Int) (CNonCanonical)]
  cec_suppress = False
  cec_defer_type_errors = TypeError
Adding error:
  test-nested.hs:20:28: error:
      • Could not deduce (Show (Pair a Int)) arising from a use of ‘show’
        from the context: ShowFst a
          bound by the instance declaration at test-nested.hs:19:10-42
      • In the expression: show x
        In an equation for ‘show’: show (MkPairWithInt x) = show x
        In the instance declaration for ‘Show (PairWithInt a)’
reportGroup
  [[WD] $dShow_a1dI {0}:: Show
                            (Pair a_a1ds[ssk:1] Int) (CNonCanonical)]
tryReporter end } Dicts False False
tryReporters } []
reportUnsolved }
reportUnsolved (unsafe overlapping) {
reportUnsolved (unsafe overlapping) }
mkTypeableBinds [PairWithInt, ShowFst, ShowPairlike, Pair]
mkTyConKindRepBinds
  PairWithInt
  * -> *
  * -> *
mkTyConKindRepBinds
  'MkPairWithInt
  forall a. Pair a Int -> PairWithInt a
  Pair a_aGu Int -> PairWithInt a_aGu
mkTyConKindRepBinds
  ShowFst
  * -> Constraint
  * -> Constraint
mkTyConKindRepBinds
  ShowPairlike
  * -> * -> Constraint
  * -> * -> Constraint
mkTyConKindRepBinds
  Pair
  * -> * -> *
  * -> * -> *
mkTyConKindRepBinds
  'MkPair
  forall a b. a -> b -> Pair a b
  a_a1bf -> b_a1bg -> Pair a_a1bf b_a1bg
Tc9
