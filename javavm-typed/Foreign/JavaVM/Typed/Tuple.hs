{--
JVM-Bridge -- bridge from FP languages and others to the Java VM
Copyright (C) 2001 Ashley Yakeley <ashley@semantic.org>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--}

module Foreign.JavaVM.Typed.Tuple () where
{    
    import Foreign.JavaVM.Typed.ArgumentList;
    import Foreign.JavaVM.Typed.Value;
    import Data.Witness;
    
    -- 0
    instance IsJVMArgumentList () where
    {
        getListType Type = [];
        tlToArgList () = return [];
        tlExtractValues [] = Just (return ());
        tlExtractValues _ = Nothing;
    };

    -- 2
    instance (IsJVMValue a,IsJVMValue b) =>
     IsJVMArgumentList (a,b) where
    {
        getListType Type = 
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b)
        ];
        tlToArgList (a,b) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b
        ];
        tlExtractValues [va,vb] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            return (do
            {
                a <- ma;
                b <- mb;
                return (a,b);
            });
        };
        tlExtractValues _ = Nothing;
    };

    -- 3
    instance (IsJVMValue a,IsJVMValue b,IsJVMValue c) =>
     IsJVMArgumentList (a,b,c) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c)
        ];
        tlToArgList (a,b,c) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c
        ];
        tlExtractValues [va,vb,vc] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                return (a,b,c);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 4
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d
    ) =>
     IsJVMArgumentList (a,b,c,d) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d)
        ];
        tlToArgList (a,b,c,d) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d
        ];
        tlExtractValues [va,vb,vc,vd] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                return (a,b,c,d);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 5
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e
    ) =>
     IsJVMArgumentList (a,b,c,d,e) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e)
        ];
        tlToArgList (a,b,c,d,e) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e
        ];
        tlExtractValues [va,vb,vc,vd,ve] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                return (a,b,c,d,e);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 6
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f)
        ];
        tlToArgList (a,b,c,d,e,f) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                return (a,b,c,d,e,f);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 7
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g)
        ];
        tlToArgList (a,b,c,d,e,f,g) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                return (a,b,c,d,e,f,g);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 8
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h)
        ];
        tlToArgList (a,b,c,d,e,f,g,h) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                return (a,b,c,d,e,f,g,h);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 9
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                return (a,b,c,d,e,f,g,h,i);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 10
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i,
        IsJVMValue j
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i,j) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i),
            getValueType (Type::Type j)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i,j) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i,
            tlArgumentToValue j
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi,vj] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            mj <- tlExtractValue vj;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                j <- mj;
                return (a,b,c,d,e,f,g,h,i,j);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 11
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i,
        IsJVMValue j,
        IsJVMValue k
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i,j,k) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i),
            getValueType (Type::Type j),
            getValueType (Type::Type k)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i,j,k) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i,
            tlArgumentToValue j,
            tlArgumentToValue k
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            mj <- tlExtractValue vj;
            mk <- tlExtractValue vk;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                j <- mj;
                k <- mk;
                return (a,b,c,d,e,f,g,h,i,j,k);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 12
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i,
        IsJVMValue j,
        IsJVMValue k,
        IsJVMValue l
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i,j,k,l) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i),
            getValueType (Type::Type j),
            getValueType (Type::Type k),
            getValueType (Type::Type l)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i,j,k,l) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i,
            tlArgumentToValue j,
            tlArgumentToValue k,
            tlArgumentToValue l
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            mj <- tlExtractValue vj;
            mk <- tlExtractValue vk;
            ml <- tlExtractValue vl;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                j <- mj;
                k <- mk;
                l <- ml;
                return (a,b,c,d,e,f,g,h,i,j,k,l);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 13
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i,
        IsJVMValue j,
        IsJVMValue k,
        IsJVMValue l,
        IsJVMValue m
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i),
            getValueType (Type::Type j),
            getValueType (Type::Type k),
            getValueType (Type::Type l),
            getValueType (Type::Type m)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i,j,k,l,m) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i,
            tlArgumentToValue j,
            tlArgumentToValue k,
            tlArgumentToValue l,
            tlArgumentToValue m
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            mj <- tlExtractValue vj;
            mk <- tlExtractValue vk;
            ml <- tlExtractValue vl;
            mm <- tlExtractValue vm;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                j <- mj;
                k <- mk;
                l <- ml;
                m <- mm;
                return (a,b,c,d,e,f,g,h,i,j,k,l,m);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 14
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i,
        IsJVMValue j,
        IsJVMValue k,
        IsJVMValue l,
        IsJVMValue m,
        IsJVMValue n
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i),
            getValueType (Type::Type j),
            getValueType (Type::Type k),
            getValueType (Type::Type l),
            getValueType (Type::Type m),
            getValueType (Type::Type n)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i,
            tlArgumentToValue j,
            tlArgumentToValue k,
            tlArgumentToValue l,
            tlArgumentToValue m,
            tlArgumentToValue n
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            mj <- tlExtractValue vj;
            mk <- tlExtractValue vk;
            ml <- tlExtractValue vl;
            mm <- tlExtractValue vm;
            mn <- tlExtractValue vn;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                j <- mj;
                k <- mk;
                l <- ml;
                m <- mm;
                n <- mn;
                return (a,b,c,d,e,f,g,h,i,j,k,l,m,n);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 15
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i,
        IsJVMValue j,
        IsJVMValue k,
        IsJVMValue l,
        IsJVMValue m,
        IsJVMValue n,
        IsJVMValue o
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i),
            getValueType (Type::Type j),
            getValueType (Type::Type k),
            getValueType (Type::Type l),
            getValueType (Type::Type m),
            getValueType (Type::Type n),
            getValueType (Type::Type o)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i,
            tlArgumentToValue j,
            tlArgumentToValue k,
            tlArgumentToValue l,
            tlArgumentToValue m,
            tlArgumentToValue n,
            tlArgumentToValue o
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn,vo] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            mj <- tlExtractValue vj;
            mk <- tlExtractValue vk;
            ml <- tlExtractValue vl;
            mm <- tlExtractValue vm;
            mn <- tlExtractValue vn;
            mo <- tlExtractValue vo;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                j <- mj;
                k <- mk;
                l <- ml;
                m <- mm;
                n <- mn;
                o <- mo;
                return (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o);
            });
        };
        tlExtractValues _ = Nothing;
    };
    
    -- 16
    instance 
    (
        IsJVMValue a,
        IsJVMValue b,
        IsJVMValue c,
        IsJVMValue d,
        IsJVMValue e,
        IsJVMValue f,
        IsJVMValue g,
        IsJVMValue h,
        IsJVMValue i,
        IsJVMValue j,
        IsJVMValue k,
        IsJVMValue l,
        IsJVMValue m,
        IsJVMValue n,
        IsJVMValue o,
        IsJVMValue p
    ) =>
     IsJVMArgumentList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    {
        getListType Type =
        [
            getValueType (Type::Type a),
            getValueType (Type::Type b),
            getValueType (Type::Type c),
            getValueType (Type::Type d),
            getValueType (Type::Type e),
            getValueType (Type::Type f),
            getValueType (Type::Type g),
            getValueType (Type::Type h),
            getValueType (Type::Type i),
            getValueType (Type::Type j),
            getValueType (Type::Type k),
            getValueType (Type::Type l),
            getValueType (Type::Type m),
            getValueType (Type::Type n),
            getValueType (Type::Type o),
            getValueType (Type::Type p)
        ];
        tlToArgList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = sequence 
        [
            tlArgumentToValue a,
            tlArgumentToValue b,
            tlArgumentToValue c,
            tlArgumentToValue d,
            tlArgumentToValue e,
            tlArgumentToValue f,
            tlArgumentToValue g,
            tlArgumentToValue h,
            tlArgumentToValue i,
            tlArgumentToValue j,
            tlArgumentToValue k,
            tlArgumentToValue l,
            tlArgumentToValue m,
            tlArgumentToValue n,
            tlArgumentToValue o,
            tlArgumentToValue p
        ];
        tlExtractValues [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn,vo,vp] = do
        {
            ma <- tlExtractValue va;
            mb <- tlExtractValue vb;
            mc <- tlExtractValue vc;
            md <- tlExtractValue vd;
            me <- tlExtractValue ve;
            mf <- tlExtractValue vf;
            mg <- tlExtractValue vg;
            mh <- tlExtractValue vh;
            mi <- tlExtractValue vi;
            mj <- tlExtractValue vj;
            mk <- tlExtractValue vk;
            ml <- tlExtractValue vl;
            mm <- tlExtractValue vm;
            mn <- tlExtractValue vn;
            mo <- tlExtractValue vo;
            mp <- tlExtractValue vp;
            return (do
            {
                a <- ma;
                b <- mb;
                c <- mc;
                d <- md;
                e <- me;
                f <- mf;
                g <- mg;
                h <- mh;
                i <- mi;
                j <- mj;
                k <- mk;
                l <- ml;
                m <- mm;
                n <- mn;
                o <- mo;
                p <- mp;
                return (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p);
            });
        };
        tlExtractValues _ = Nothing;
    };
}
