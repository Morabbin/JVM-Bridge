default: install.javavm-typed

install.javavm:
	cd javavm && make install

install.javavm-interface.Haskell: install.javavm
	cd javavm-interface/Haskell && make install && cd test && make

install.javavm-typed: install.javavm-interface.Haskell
	cd javavm-typed && make install && cd test && make

clean:
	cd javavm-typed && make clean
	cd javavm-interface/Haskell && make clean
	cd javavm && make clean
