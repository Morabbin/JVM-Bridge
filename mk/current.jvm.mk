JVM_ADDRESSSIZE=64
JVM_BASEDIR=/System/Library/Frameworks
# Sun JRE/JDK as shipped with Darwin for AWT in Aqua
# This will pick the latest version in the framework
JVM_NAME=Darwin
JVM_FRAMEWORKDIR=$(JVM_BASEDIR)
JVM_FRAMEWORK=JavaVM
JVM_FWDIR=$(JVM_BASEDIR)/$(JVM_FRAMEWORK).framework
JVM_INCLUDES=$(JVM_FWDIR)/Headers
JVM_LIBDIR=
JVM_LIBDIRS=
JVM_BINDIR=$(JVM_FWDIR)/Commands
JVM_CLASSES=$(shell cat $(JVM_LIBDIR)/classlist)
JVM_CPPFLAGS=-DVM_SUN
JVM_LDFLAGS=-lobjc
JVM_CHECKFILE=$(JVM_FWDIR)/Headers/jni.h
JAVAC=$(JVM_BINDIR)/javac
JAVACFLAGS=
JAVAH=$(JVM_BINDIR)/javah
JAVAHFLAGS=-jni -classpath ./
JAR=$(JVM_BINDIR)/jar
