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

module Foreign.JavaVM.Typed.Callback where
    {
    import Foreign.JavaVM.Typed.Method;
    import Foreign.JavaVM.Typed.Class;
    import Foreign.JavaVM.Typed.Tuple();
    import Foreign.JavaVM.Typed.Primitive();
    import Foreign.JavaVM.Typed.Returnable;
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.Typed.Value;
    import Foreign.JavaVM.Typed.ArgumentList;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Control.Monad.Loop;
    import Control.Concurrent;
    import Control.Exception;
    import Data.Witness;
    
    startExecuteFunction :: VM ();
    startExecuteFunction = vmStartExecuteFunction; 

    tlDefineCallbackClass :: VMRef -> ClassName -> ClassName -> [ClassName] -> [ValueType] -> [MethodNameType] -> VM JClass;
    tlDefineCallbackClass loader classname superclassname interfaces superclassconstructor methods = do
        {
        startExecuteFunction;
        vmDefineCallbackClass loader classname superclassname interfaces superclassconstructor methods;
        findClassByName classname; -- this is curiously necessary
        };

    reportArgError :: Maybe (VM t) -> VM t;
    reportArgError (Just vmt) = vmt;
    reportArgError _ = fail "callback: args: bad list";

    makeCallback :: (IsJVMReference c,IsJVMArgumentList a,IsJVMReturnable r) => (c -> a -> VM r) -> VM OpaqueAddress;
    makeCallback = makeCallback' Type where
    {
        makeCallback' :: (IsJVMReference c,IsJVMArgumentList a,IsJVMReturnable r) => Type a -> (c -> a -> VM r) -> VM OpaqueAddress;
        makeCallback' t foo = tlMakeCallback (MkAnyWitness Tref:getArgTypes t) (\vobjargs -> do
        {
            case vobjargs of
            {
                (vobj:vargs) -> do
                {
                    obj <- reportArgError (tlExtractValue vobj);
                    args <- reportArgError (tlExtractValues vargs);
                    foo obj args;
                };
                _ -> fail "callback: args: no object";
            };
        });
    };

    makeStaticCallback :: (IsJVMArgumentList a,IsJVMReturnable r) => (a -> VM r) -> VM OpaqueAddress;
    makeStaticCallback = makeStaticCallback' Type where
    {
        makeStaticCallback' :: (IsJVMArgumentList a,IsJVMReturnable r) => Type a -> (a -> VM r) -> VM OpaqueAddress;
        makeStaticCallback' t foo = tlMakeCallback (getArgTypes t) (\vargs -> do
        {
            args <- reportArgError (tlExtractValues vargs);
            foo args;
        });
    };
    
    getExecuteFunctionClass :: VM JClass;
    getExecuteFunctionClass = do
        {
        startExecuteFunction; 
        findClassByName executeFunctionClassName;
        };
    
    getprocPending :: JClass -> VM (() -> IO Jboolean);
    getprocPending execClass = getStaticMethod execClass "procPending";
    
    getdoNextProc ::  JClass -> VM (() -> IO ());
    getdoNextProc execClass = getStaticMethod execClass "doNextProc";

    runProcs :: VM Jboolean -> VM () -> VM Bool -> VM ();
    runProcs procPending doNextProc stopNow = do
        {
        stop <- stopNow;
        if (stop) then (return ()) else (do
            {
            pending <- procPending;
            if (pending) then (do
                {
                forkIO (do
                    {
                    result <- doNextProc;
                    return result;
                    });
                return ();
                }) else (return ());
            Control.Concurrent.yield;
            runProcs procPending doNextProc stopNow; 
            });
        };
    
    -- returns the 'stop' proc.
    startProcsThread :: VM (IO ());
    startProcsThread = do
        {
        executeClass    <- getExecuteFunctionClass;
        procPending        <- getprocPending executeClass;
        doNextProc        <- getdoNextProc executeClass;
        stopVar            <- newEmptyMVar;
        forkIO 
            (runProcs (procPending ()) (doNextProc ()) (isEmptyMVar stopVar >>= (return .  not)));
        return (tryPutMVar stopVar () >> return ());
        };
    
    withProcsThread :: VM a -> VM a;
    withProcsThread f = bracket startProcsThread id (const f);
    
    tlYieldLoop :: (() -> VM ()) -> VM Bool -> VM ();
    tlYieldLoop yieldMethod stopNow = do
        {
        while (do
            {
            stop <- stopNow;
            if (stop)
             then (return True)
             else (do
                {
                yieldMethod ();
                Control.Concurrent.yield;
                return False;
                });
            });
        };
    }
