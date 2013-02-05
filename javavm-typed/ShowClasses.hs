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

module Main where
    {
    import Foreign.JavaVM.Typed;
    import Platform.JavaVM;
    import Control.Monad.Reference;
    import Control.Monad.Loop;
    import Data.String;
    import Data.Nothing;
    import Data.Witness;
    import Data.Bits;
    import System.Environment;
    import System.IO;
    import System.Exit;


    data JObjectClass;
    instance IsJavaClassMarker JObjectClass where
        {cName Type = "java/lang/Object";};
    type JObject = ObjectRef JObjectClass;
    class IsJObject c where
        {toJObject :: c -> JObject;};
    instance IsJObject JObject where
        {toJObject = castTLRef;};

    instance IsJObject JClass where
        {toJObject = castTLRef;};

    data JReaderClass;
    instance IsJavaClassMarker JReaderClass where
        {cName Type = "java/io/Reader";};
    type JReader = ObjectRef JReaderClass;
    class IsJReader c where
        {toJReader :: c -> JReader;};
    instance IsJReader JReader where
        {toJReader = castTLRef;};
    instance IsJObject JReader where
        {toJObject = castTLRef;};

    data JBufferedReaderClass;
    instance IsJavaClassMarker JBufferedReaderClass where
        {cName Type = "java/io/BufferedReader";};
    type JBufferedReader = ObjectRef JBufferedReaderClass;
    instance IsJReader JBufferedReader where
        {toJReader = castTLRef;};
    instance IsJObject JBufferedReader where
        {toJObject = castTLRef;};

    data JInputStreamReaderClass;
    instance IsJavaClassMarker JInputStreamReaderClass where
        {cName Type = "java/io/InputStreamReader";};
    type JInputStreamReader = ObjectRef JInputStreamReaderClass;
    instance IsJReader JInputStreamReader where
        {toJReader = castTLRef;};
    instance IsJObject JInputStreamReader where
        {toJObject = castTLRef;};

    data JInputStreamClass;
    instance IsJavaClassMarker JInputStreamClass where
        {cName Type = "java/io/InputStream";};
    type JInputStream = ObjectRef JInputStreamClass;
    instance IsJObject JInputStream where
        {toJObject = castTLRef;};

    data JPrintStreamClass;
    instance IsJavaClassMarker JPrintStreamClass where
        {cName Type = "java/io/PrintStream";};
    type JPrintStream = ObjectRef JPrintStreamClass;
    instance IsJObject JPrintStream where
        {toJObject = castTLRef;};

    data JSystemClass;
    instance IsJavaClassMarker JSystemClass where
        {cName Type = "java/lang/System";};
    type JSystem = ObjectRef JSystemClass;
    instance IsJObject JSystem where
        {toJObject = castTLRef;};

    data JMemberClass;
    instance IsJavaClassMarker JMemberClass where
        {cName Type = "java/lang/reflect/Member";};
    type JMember = ObjectRef JMemberClass;
    class IsJMember c where
        {toJMember :: c -> JMember;};
    instance IsJMember JMember where
        {toJMember = castTLRef;};
    instance IsJObject JMember where
        {toJObject = castTLRef;};

    data JMethodClass;
    instance IsJavaClassMarker JMethodClass where
        {cName Type = "java/lang/reflect/Method";};
    type JMethod = ObjectRef JMethodClass;
    instance IsJMember JMethod where
        {toJMember = castTLRef;};
    instance IsJObject JMethod where
        {toJObject = castTLRef;};

    data JConstructorClass;
    instance IsJavaClassMarker JConstructorClass where
        {cName Type = "java/lang/reflect/Constructor";};
    type JConstructor = ObjectRef JConstructorClass;
    instance IsJMember JConstructor where
        {toJMember = castTLRef;};
    instance IsJObject JConstructor where
        {toJObject = castTLRef;};

    data JFieldClass;
    instance IsJavaClassMarker JFieldClass where
        {cName Type = "java/lang/reflect/Field";};
    type JField = ObjectRef JFieldClass;
    instance IsJMember JField where
        {toJMember = castTLRef;};
    instance IsJObject JField where
        {toJObject = castTLRef;};


    type JVM a = VM a;


    runJVMOnArgClasspath :: VM () -> IO ();
    runJVMOnArgClasspath foo = catch (do
        {
        classpath <- getArgs;
        runVMReport classpath foo;
        })
        (\ex -> (do
        {
        putStrLn ("caught exception: " ++ (show ex));
        exitFailure;
        }));

    forAny :: (Monad m) => [a] -> (a -> m Bool) -> m Bool;
    forAny [] _ = return False;
    forAny (a:as) getMatch = do
        {
        isA <- getMatch a;
        if isA then (return True) else (forAny as getMatch);
        };

    foreachgen :: (Monad m) => c -> (b -> c -> m c) -> [a] -> (a -> m b) -> m c;
    foreachgen nn _ [] _ = return nn;
    foreachgen nn gather (a:as) f = do
        {
        b <- f a;
        c <- foreachgen nn gather as f;
        gather b c;
        };

    foreach :: (Monad m) => [a] -> (a -> m ()) -> m ();
    foreach = foreachgen () (\() () -> return ());

    foreachres :: (Monad m) => [a] -> (a -> m b) -> m [b];
    foreachres = foreachgen [] (\b c -> return (b:c));

    foreachcollect :: (Monad m) => [a] -> (a -> m [b]) -> m [b];
    foreachcollect = foreachgen [] (\b c -> return (b ++ c));

    mergeOne :: (Monad m) => (a -> a -> m Bool) -> [a] -> a -> m [a];
    mergeOne eqF pl q = do
        {
        has <- forAny pl (\p -> eqF p q);
        return (if has
         then pl
         else (q:pl)
            );
        };

    merge :: (Monad m) => (a -> a -> m Bool) -> [a] -> [a] -> m [a];
    merge _ pl [] = return pl;
    merge eqF pl (q:qs) = do
        {
        has <- forAny pl (\p -> eqF p q);
        if has
         then (merge eqF pl qs)
         else do
            {
            pq <- merge eqF pl qs;
            return (q:pq);
            };
        };

    foreachmerge :: (Monad m) => (b -> b -> m Bool) -> [a] -> (a -> m [b]) -> m [b];
    foreachmerge eqF = foreachgen [] (merge eqF);

    isWS :: Jchar -> Bool;
    isWS c = c <= 32;

    firstNWS :: JavaString -> (JavaString,JavaString);
    firstNWS [] = ([],[]);
    firstNWS (c:cs) | isWS c = ([],cs);
    firstNWS (c:cs) = let {(f,r) = firstNWS cs} in (c:f,r);

    trimWS :: JavaString -> JavaString;
    trimWS [] = [];
    trimWS (c:cs) | isWS c = trimWS cs;
    trimWS s = s;

    splitWS :: JavaString -> [JavaString];
    splitWS s = case firstNWS (trimWS s) of
        {
        ([],_) -> [];
        (f,r) -> f:(splitWS r);
        };

    slashToDot :: Jchar -> Jchar;
    slashToDot 47 = 46;
    slashToDot c = c;

    dotToSlash :: Jchar -> Jchar;
    dotToSlash 46 = 47;
    dotToSlash c = c;

    main :: IO ();
    main = runJVMOnArgClasspath (do
        {
        objectClass                <- findClass (Type :: Type JObject);
        (equalsMethod :: JObject -> (JObject) -> IO Jboolean)
                                <- getMethod objectClass "equals";

        systemClass                <- findClass (Type :: Type JSystem);
        (inRef :: Ref IO JInputStream)
                                <- getStaticField systemClass "in";
        --(outRef :: Ref JVM JPrintStream)
        --                        <- getStaticField systemClass "out";

        stringClass                <- findClass (Type :: Type JString);
        (toCharArray :: JString -> () -> IO [Jchar])
                                <- getMethod stringClass "toCharArray";
        --(makeNewString :: ([Jchar]) -> JVM JString)
        --                        <- getMakeNewObject stringClass;
        
        readerClass                <- findClass (Type :: Type JReader);
        (close :: JReader -> () -> IO ())
                                <- getMethod readerClass "close";

        --fileInputStreamClass    <- findClassByName "java/io/FileInputStream";
        --(makeNewFileInputStream :: JString -> JVM JInputStream)
        --                        <- getMakeNewObject fileInputStreamClass;

        inputStreamReaderClass    <- findClass (Type :: Type JInputStreamReader);
        (makeNewInputStreamReader :: JInputStream -> IO JInputStreamReader)
                                <- getMakeNewObject inputStreamReaderClass;

        bufferedReaderClass        <- findClass (Type :: Type JBufferedReader);
        (makeNewBufferedReader :: JReader -> IO JBufferedReader)
                                <- getMakeNewObject bufferedReaderClass;
        (readLine :: JBufferedReader -> () -> IO JString)
                                <- getMethod bufferedReaderClass "readLine";

        --printStreamClass        <- findClass (Type :: Type JPrintStream);
        --(println :: JPrintStream -> ([Jchar]) -> JVM Jvoid)
        --                        <- getMethod printStreamClass "println";        

        classClass                <- findClass (Type :: Type JClass);
        --(forName :: (JString) -> JVM JClass)
        --                        <- getStaticMethod classClass "forName";        
        (getName :: JClass -> () -> IO JString)
                                <- getMethod classClass "getName";
        (getClassModifiers :: JClass -> () -> IO Jint)
                                <- getMethod classClass "getModifiers";
        (getInterfaces :: JClass -> () -> IO [JClass])
                                <- getMethod classClass "getInterfaces";
        (getSuperclass :: JClass -> () -> IO JClass)
                                <- getMethod classClass "getSuperclass";
        (getDeclaredFields :: JClass -> () -> IO [JField])
                                <- getMethod classClass "getDeclaredFields";
        (getDeclaredMethods :: JClass -> () -> IO [JMethod])
                                <- getMethod classClass "getDeclaredMethods";
        (getDeclaredConstructors :: JClass -> () -> IO [JConstructor])
                                <- getMethod classClass "getDeclaredConstructors";
        (getDeclaredMethod :: JClass -> (JString,[JClass]) -> IO JMethod)
                                <- getMethod classClass "getDeclaredMethod";

        constructorClass        <- findClass (Type :: Type JConstructor);
        (constructorgetParameterTypes :: JConstructor -> () -> IO [JClass])
                                <- getMethod constructorClass "getParameterTypes";
        
        memberClass                <- findClass (Type :: Type JMember);
        (membergetName :: JMember -> () -> IO JString)
                                <- getMethod memberClass "getName";
        (getModifiers :: JMember -> () -> IO Jint)
                                <- getMethod memberClass "getModifiers";
        
        fieldClass                <- findClass (Type :: Type JField);
        (jgetType :: JField -> () -> IO JClass)
                                <- getMethod fieldClass "getType";
        
        methodClass                <- findClass (Type :: Type JMethod);
        (getReturnType :: JMethod -> () -> IO JClass)
                                <- getMethod methodClass "getReturnType";
        (getParameterTypes :: JMethod -> () -> IO [JClass])
                                <- getMethod methodClass "getParameterTypes";
        
        
        classesIS    <- get inRef;
        classesISR    <- makeNewInputStreamReader classesIS;
        classesBR    <- makeNewBufferedReader (toJReader classesISR);
        
        --outStream    <- get outRef;
        
        let
            {
            outputLine :: JavaString -> JVM ();
--            outputLine s = return ();
--            outputLine s = println outStream s;
--            outputLine s = putStrLn (showJavaString s);
            outputLine s = do
                {
                putStrLn (showJavaString s);
                hFlush stdout;
                };
    
            commaList :: [JavaString] -> JavaString;
            commaList [] = [];
            commaList (x:[]) = x;
            commaList (x:xs) = x ++ "," ++ (commaList xs);
            
            parameterList :: [JavaString] -> JavaString;
            parameterList ss = "(" ++ (commaList ss) ++ ")";
            
            -- m4-style quoting
            enquote :: JavaString -> JavaString;
            enquote x = "`" ++ x ++ "'";

            mkCmd :: JavaString -> [JavaString] -> JavaString;
            mkCmd cmd ss = cmd ++ (parameterList (fmap enquote ss));
    
            outputCmd :: JavaString -> [JavaString] -> JVM ();
            outputCmd cmd ss = outputLine (mkCmd cmd ss);

            equalsObj :: (IsJObject a,IsJObject b) => a -> (b) -> JVM Jboolean;
            equalsObj a b = equalsMethod (toJObject a) (toJObject b);

            getNameArray :: JClass -> JVM JavaString;
            getNameArray c = do
                {
                str <- getName c ();
                rawName <- toCharArray str ();
                return (fixClassName rawName);
                } where
                {
                fixClassName :: JavaString -> JavaString;
                fixClassName ss@(ff:_) | ff == (toJChar '[')        = typeNameFromSig ss where
                    {
                    typeNameFromSig :: JavaString -> JavaString;
                    typeNameFromSig (f:[])    | f == (toJChar 'V')    = "void";
                    typeNameFromSig (f:[])    | f == (toJChar 'Z')    = "boolean";
                    typeNameFromSig (f:[])    | f == (toJChar 'B')    = "byte";
                    typeNameFromSig (f:[])    | f == (toJChar 'C')    = "char";
                    typeNameFromSig (f:[])    | f == (toJChar 'S')    = "short";
                    typeNameFromSig (f:[])    | f == (toJChar 'I')    = "int";
                    typeNameFromSig (f:[])    | f == (toJChar 'J')    = "long";
                    typeNameFromSig (f:[])    | f == (toJChar 'F')    = "float";
                    typeNameFromSig (f:[])    | f == (toJChar 'D')    = "double";
                    typeNameFromSig (f:s)    | (f == (toJChar 'L')) && ((last s) == (toJChar ';'))    = take ((length s) - 1) s;
                    typeNameFromSig (f:s)    | f == (toJChar '[')    = "[" ++ (typeNameFromSig s) ++ "]";
                    typeNameFromSig _ = error "unmatched typeNameFromSig";
                    };
                fixClassName s = s;
                };
            
            foundInClasses :: [JClass] -> JString -> [JClass] -> JVM Bool;
            foundInClasses classes mName mTypes = forAny classes foundInClass where
                {                
                foundInClass :: JClass -> JVM Bool;
                foundInClass cl = do
                    {
                    theMethod <- catchJThrowable (getDeclaredMethod cl (mName,mTypes)) (\_ -> getNothing);
                    noMethod <- getIsNothing theMethod;
                    return (not noMethod);
                    };
                };
            
            outputClass :: JavaString -> JVM ();
            outputClass classname = do
                {
                theClass <- catchJThrowable (findClassByName (fmap dotToSlash classname)) (\_ -> getNothing);
                noClass <- getIsNothing theClass;
                if (noClass)
                 then do
                     {
                    outputLine [];
                    outputLine ("dnl " ++ classname ++ " not found");
                    hPutStrLn stderr ("ShowClasses: " ++ (showJavaString classname) ++ " not found");
                    hFlush stderr;
                    exitFailure;
                     }
                 else do
                    {
                    cmodifiers <- getClassModifiers theClass ();
                    -- public classes only
                    if (cmodifiers .&. 1 > 0) then do
                        {
                        cname <- getNameArray theClass;
                        outputLine [];
                        outputLine (("dnl ") ++ cname);
                        outputCmd "sc_classname" [cname];

                        -- superclasses and interfaces
                        supers <- let
                            {
                            newInterfaces :: [JClass] -> JClass -> JVM [JClass];
                            newInterfaces intfs aClass = do
                                {
                                interfaces <- getInterfaces aClass ();
                                foreachmerge equalsObj interfaces (\iClass -> do
                                    {
                                    already <- forAny intfs (\intf -> equalsObj intf iClass);
                                    if already
                                     then (return [])
                                     else do
                                        {
                                        iSupers <- newInterfaces intfs iClass;
                                        return (iClass:iSupers);
                                        };
                                    });
                                };
                            addSupers :: [JClass] -> JClass -> JVM [JClass];
                            addSupers intfs aClass = do
                                {
                                interfaces <- newInterfaces intfs aClass;
                                sClass <- getSuperclass aClass ();
                                noSClass <- getIsNothing sClass;
                                if noSClass
                                 then (return (intfs ++ interfaces))
                                 else do
                                    {
                                    sSupers <- addSupers (intfs ++ interfaces) sClass;
                                    return (sClass:sSupers);
                                    };
                                };
                            }
                        in addSupers [] theClass;
                        
                        foreach supers (\cl -> do
                            {
                            scname <- getNameArray cl;
                            outputCmd "sc_subclass" [cname,scname];
                            });
                        
                        -- fields
                        fields <- getDeclaredFields theClass ();
                        foreach fields (\field -> do
                            {
                            fmodifiers <- getModifiers (toJMember field) ();
                            -- public only
                            if (fmodifiers .&. 1 > 0) then do
                                {
                                jname <- membergetName (toJMember field) ();
                                fname <- toCharArray jname ();
                                ftype <- jgetType field ();
                                ftname <- getNameArray ftype;
                                outputCmd
                                 (if (fmodifiers .&. 8 > 0) then "sc_staticfield" else "sc_field")
                                 [classname,fname,ftname];
                                }
                             else (return ());
                            });
                        
                        -- methods
                        methods <- getDeclaredMethods theClass ();
                        foreach methods (\method -> do
                            {
                            modifiers <- getModifiers (toJMember method) ();
                            -- public only
                            if (modifiers .&. 1 > 0) then do
                                {
                                let {isStatic = modifiers .&. 8 > 0};
                                methodName <- membergetName (toJMember method) ();
                                methodPTypes <- getParameterTypes method ();
                                inherited <- if (isStatic) then (return False) else (foundInClasses supers methodName methodPTypes);
                                if (inherited)
                                 then (return ())
                                 else do
                                    {
                                    name <- toCharArray methodName ();
                                    mtype <- getReturnType method ();
                                    mtname <- getNameArray mtype;
                                    pnames <- foreachres methodPTypes getNameArray;
                                    outputCmd
                                     (if isStatic then "sc_staticmethod" else "sc_method")
                                     [classname,name,mtname,parameterList pnames];
                                    };
                                }
                             else (return ());
                            });
                        
                        -- constructors
                        constructors <- getDeclaredConstructors theClass ();
                        foreach constructors (\method -> do
                            {
                            mmodifiers <- getModifiers (toJMember method) ();
                            -- public only
                            if (mmodifiers .&. 1 > 0) then do
                                {
                                -- let {isStatic = mmodifiers .&. 8 > 0};
                                methodPTypes <- constructorgetParameterTypes method ();
                                pnames <- foreachres methodPTypes getNameArray;
                                outputCmd "sc_constructor" [classname,parameterList pnames];
                                }
                             else (return ());
                            });

                        outputCmd "sc_endclass" [cname];
                        }
                    else do
                        {
                        hPutStrLn stderr ("ShowClasses: " ++ (showJavaString classname) ++ " not public");
                        hFlush stderr;
                        };
                    };
                };
            };
        while (do
            {
            jClassNamesLine <- readLine classesBR ();
            stop <- getIsNothing jClassNamesLine;
            if (stop)
             then (return False)
             else do
                {
                classNamesLine <- toCharArray jClassNamesLine ();
                mapM_ (outputClass . (fmap slashToDot)) (splitWS classNamesLine);
                return True;
                };
            });
        
        close (toJReader classesBR) ();

        return ();
        } :: JVM ());
    }
