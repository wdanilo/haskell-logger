{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Log.Tuples where

type family Tuple2RTuple a where
    Tuple2RTuple ()                               = ()
    Tuple2RTuple (t1,t2)                          = (t1,(t2,()))
    Tuple2RTuple (t1,t2,t3)                       = (t1,(t2,(t3,())))
    Tuple2RTuple (t1,t2,t3,t4)                    = (t1,(t2,(t3,(t4,()))))
    Tuple2RTuple (t1,t2,t3,t4,t5)                 = (t1,(t2,(t3,(t4,(t5,())))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6)              = (t1,(t2,(t3,(t4,(t5,(t6,()))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7)           = (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7,t8)        = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7,t8,t9)     = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))
    Tuple2RTuple t                                = (t,())

type family RTuple2Tuple a where
    RTuple2Tuple ()                                                    = ()
    RTuple2Tuple (t1,(t2,()))                                          = (t1,t2)
    RTuple2Tuple (t1,(t2,(t3,())))                                     = (t1,t2,t3)
    RTuple2Tuple (t1,(t2,(t3,(t4,()))))                                = (t1,t2,t3,t4)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,())))))                           = (t1,t2,t3,t4,t5)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,()))))))                      = (t1,t2,t3,t4,t5,t6)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))                 = (t1,t2,t3,t4,t5,t6,t7)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))            = (t1,t2,t3,t4,t5,t6,t7,t8)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))       = (t1,t2,t3,t4,t5,t6,t7,t8,t9)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,())))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
    RTuple2Tuple (t,())                                                = t



type family Insert t set where
  Insert t ()    = (t,())
  Insert t (t,x) = (t,x)
  Insert t (a,x) = (a,Insert t x)


type family MapRTuple (f :: * -> *) tup where
    MapRTuple f () = ()
    MapRTuple f (a,as) = (f a, MapRTuple f as)

class MapRTuple2 f tup tup' | f tup -> tup'
    where mapRTuple :: f -> tup -> tup'

instance MapRTuple2 f () () where
    mapRTuple _ = id

instance MapRTuple2 (a -> b) as bs => MapRTuple2 (a -> b) (a,as) (b,bs) where
    mapRTuple f (a,as) = (f a, mapRTuple f as)