module ExtendedTuples where

frst (a,_,_) = a
scnd (_,b,_) = b
thrd (_,_,c) = c

frst4 (a,_,_,_) = a
scnd4 (_,b,_,_) = b
thrd4 (_,_,c,_) = c
frth4 (_,_,_,d) = d

frst5 (a,_,_,_,_) = a
scnd5 (_,b,_,_,_) = b
thrd5 (_,_,c,_,_) = c
frth5 (_,_,_,d,_) = d
ffth5 (_,_,_,_,e) = e
