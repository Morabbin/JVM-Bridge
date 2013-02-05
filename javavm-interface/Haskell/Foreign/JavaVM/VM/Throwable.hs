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

module Foreign.JavaVM.VM.Throwable(checkCall,vmThrow,vmCatch) where
{
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;
    import Control.Exception;

    exceptionPending :: VM Jboolean;
    exceptionPending = do
    {
        xp <- rawExceptionPending ?jvmenv;
        return (fromRawBoolean xp); 
    };
    
    checkCall :: VM a -> VM a;
    checkCall foo = do
    {
        result <- foo;
        ex <- exceptionPending;
        if ex 
        then ioError (userError "pending VM exception")
        else return result;
    };

    vmThrow :: VMRef -> VM a;
    vmThrow th = do
    {
        _ <- checkCall (withVMRefRawGlobal th (\rawTh -> rawThrowException ?jvmenv rawTh));
        fail "couldn't throw Java VM exception";
    };

    getClearException :: VM VMRef;
    getClearException = do
    {
        x <- rawGetClearException ?jvmenv;
        makeVMRefFromLocal x; 
    };

    vmCatch :: VM a -> (VMRef -> VM a) -> VM a;
    vmCatch foo catchClause = Control.Exception.catch foo (\x -> do
    {
        pending <- exceptionPending;
        if pending
        then do
        {
            exRef <- getClearException;
            catchClause exRef;
        }
        else ioError x;
    });
}
