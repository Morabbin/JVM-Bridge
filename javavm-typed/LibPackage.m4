dnl JVM-Bridge -- bridge from FP languages and others to the Java VM
dnl Copyright (C) 2001 Ashley Yakeley <ashley@semantic.org>
dnl
dnl This library is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU Lesser General Public
dnl License as published by the Free Software Foundation; either
dnl version 2.1 of the License, or (at your option) any later version.
dnl
dnl This library is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Lesser General Public License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public
dnl License along with this library; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
dnl
dnl
define(`first_lower',`regexp(`$1',`^\(.\)\(.*\)',`translit(`\1',`A-Z',`a-z')`\2'')')dnl
define(`id_module',`patsubst(`$1',`\.m4$',`')')dnl
define(`name_short',`patsubst(patsubst(`$1',`^.*\.\([^.]+\)$',`\1'),`\$',`\_')')dnl
define(`name_long',`patsubst(`$1',`[\.\$]',`\_')')dnl
define(`first_J',`patsubst(`$1',`[A-Za-z0-9_.]+',`J`'\&')')dnl
define(`id_type_class',`first_J(name_long(`$1'))')dnl
define(`id_type_short',`first_J(name_short(`$1'))')dnl
define(`id_type_primitive',`first_J(`$1')')dnl
define(`id_type',`ifelse(regexp(`$1',`\.'),`-1',id_type_primitive(`$1'),id_type_class(`$1'))')dnl
define(`id_class_IsType',`Is`'id_type_class(`$1')')dnl
define(`arg_list',`patsubst(`$1',`[A-Za-z0-9_.$]+',`id_type(\&)')')dnl
dnl
define(`id_pp_type',`ifelse(regexp(`$1',`\.'),`-1',id_type_primitive(`$1'),id_type_short(`$1'))')dnl
dnl define(`id_pp_array',`patsubst(`$1',`\[\([^\[\]]+\)\]',`Array`'id_pp_array(\&)')')dnl
define(`id_pp_array',`patsubst(patsubst(`$1',`\[',`Array'),`\]',`')')dnl
define(`id_pp_arg_list',`patsubst(patsubst(patsubst(id_pp_array(patsubst(`$1',`[A-Za-z0-9_.$]+',`id_pp_type(\&)')),`[,]',`_'),`[()]',`'),`^.',`_\&')')dnl
dnl
define(`id_pp_field',``$2'_`'id_type_short(`$1')`'')dnl
define(`id_pp_method',`id_pp_field(`$1',`$2')`'id_pp_arg_list(`$3')')dnl
define(`id_part_class',`Class_`'id_type_short(`$1')')dnl
define(`id_part_method',`Method_`'id_pp_method(`$1',`$2',`$3')')dnl
define(`id_part_staticmethod',`StaticMethod_`'id_pp_method(`$1',`$2',`$3')')dnl
define(`id_part_field',`Field_`'id_pp_field(`$1',`$2')')dnl
define(`id_part_staticfield',`StaticField_`'id_pp_field(`$1',`$2')')dnl
define(`id_part_constructor',`New_`'id_type_short(`$1')`'id_pp_arg_list(`$2')')dnl
dnl
define(`id_val_toType',`to`'id_type_class(`$1')')dnl
define(`id_val_load_class',`load`'id_part_class(`$1')')dnl
define(`id_val_load_field',`load`'id_part_field(`$1',`$2')')dnl
define(`id_val_load_staticfield',`load`'id_part_staticfield(`$1',`$2')')dnl
define(`id_val_load_method',`load`'id_part_method(`$1',`$2',`$3')')dnl
define(`id_val_load_staticmethod',`load`'id_part_staticmethod(`$1',`$2',`$3')')dnl
define(`id_val_load_constructor',`load`'id_part_constructor(`$1',`$2')')dnl
define(`id_val_load_field',`load`'id_part_field(`$1',`$2')')dnl
define(`id_val_load_staticfield',`load`'id_part_staticfield(`$1',`$2')')dnl
dnl
define(`id_type_bundle',`Bundle_`'id_type_short(`$1')')dnl
define(`id_cons_bundle',`Mk`'id_type_bundle(`$1')')dnl
define(`id_class_hasbundle',`Has`'id_type_bundle(`$1')')dnl
define(`id_val_getbundle',`get`'id_type_bundle(`$1')')dnl
define(`id_val_getfrombundle_class',`bundle`'id_part_class(`$1')')dnl
define(`id_val_getfrombundle_method',`bundle`'id_part_method(`$1',`$2',`$3')')dnl
define(`id_val_getfrombundle_staticmethod',`bundle`'id_part_staticmethod(`$1',`$2',`$3')')dnl
define(`id_val_getfrombundle_constructor',`bundle`'id_part_constructor(`$1',`$2')')dnl
define(`id_val_getfrombundle_field',`bundle`'id_part_field(`$1',`$2')')dnl
define(`id_val_getfrombundle_staticfield',`bundle`'id_part_staticfield(`$1',`$2')')dnl
dnl
define(`id_val_class',`class_`'id_type_short(`$1')')dnl
define(`id_val_method',`first_lower(id_pp_method(`$1',`$2',`$3'))')dnl
define(`id_val_staticmethod',`first_lower(id_pp_method(`$1',`$2',`$3'))')dnl
define(`id_val_constructor',`first_lower(id_pp_method(`$1',`new',`$2'))')dnl
define(`id_val_field',`first_lower(id_pp_field(`$1',`$2'))')dnl
define(`id_val_staticfield',`first_lower(id_pp_field(`$1',`$2'))')dnl
dnl
define(`sc_subclass',`	instance id_class_IsType(`$2') id_type_class(`$1') where
		{id_val_toType(`$2') = TypedLayer.castTLRef;};')dnl
dnl
define(`sc_classname',`

	-- `$1'

	type id_type_short(`$1') = id_type_class(`$1');

	id_val_load_class(`$1') :: (TypedLayer.IsJVMMonad m) => m TypedLayer.JClass;
	id_val_load_class(`$1') = TypedLayer.findClass (Type :: Type id_type_short(`$1'));

	to`'id_type_short(`$1') :: (id_class_IsType(`$1') c) => c -> id_type_short(`$1');
	to`'id_type_short(`$1') = id_val_toType(`$1');

sc_subclass(`$1',`$1')')dnl
dnl
define(`sc_staticmethod',`
divert(1)dnl
		,id_val_getfrombundle_staticmethod(`$1',`$2',`$4')	:: arg_list(`$4') -> m id_type(`$3')
divert(2)dnl
			id_val_staticmethod(`$1',`$2',`$4') <- id_val_load_staticmethod(`$1',`$2',`$4') theClass;
divert(3)dnl
				id_val_staticmethod(`$1',`$2',`$4')
divert(4)dnl

	id_val_staticmethod(`$1',`$2',`$4') :: (id_class_hasbundle(`$1') m) => arg_list(`$4') -> m id_type(`$3');
	id_val_staticmethod(`$1',`$2',`$4') args = id_val_getbundle(`$1') >>= (\bundle -> id_val_getfrombundle_staticmethod(`$1',`$2',`$4') bundle args);
divert`'dnl
	id_val_load_staticmethod(`$1',`$2',`$4') :: (TypedLayer.IsJVMMonad m,TypedLayer.IsJVMMonad mm) =>
		TypedLayer.JClass ->
		m (arg_list(`$4') -> mm id_type(`$3'));
	id_val_load_staticmethod(`$1',`$2',`$4') classRef = TypedLayer.getStaticMethod classRef "`$2'";
')dnl
dnl
define(`sc_method',`
divert(1)dnl
		,id_val_getfrombundle_method(`$1',`$2',`$4')	:: id_type_short(`$1') -> arg_list(`$4') -> m id_type(`$3')
divert(2)dnl
			id_val_method(`$1',`$2',`$4') <- id_val_load_method(`$1',`$2',`$4') theClass;
divert(3)dnl
				id_val_method(`$1',`$2',`$4')
divert(4)dnl

	id_val_method(`$1',`$2',`$4') :: (id_class_hasbundle(`$1') m,id_class_IsType(`$1') c) => c -> arg_list(`$4') -> m id_type(`$3');
	id_val_method(`$1',`$2',`$4') obj args = id_val_getbundle(`$1') >>= (\bundle -> id_val_getfrombundle_method(`$1',`$2',`$4') bundle (id_val_toType(`$1') obj) args);
divert`'dnl
	id_val_load_method(`$1',`$2',`$4') :: (TypedLayer.IsJVMMonad m,TypedLayer.IsJVMMonad mm) =>
		TypedLayer.JClass ->
		m (id_type_class(`$1') -> arg_list(`$4') -> mm id_type(`$3'));
	id_val_load_method(`$1',`$2',`$4') classRef = TypedLayer.getMethod classRef "`$2'";
')dnl
define(`sc_constructor',`
divert(1)dnl
		,id_val_getfrombundle_constructor(`$1',`$2')	:: arg_list(`$2') -> m id_type(`$1')
divert(2)dnl
			id_val_constructor(`$1',`$2') <- id_val_load_constructor(`$1',`$2') theClass;
divert(3)dnl
				id_val_constructor(`$1',`$2')
divert(4)dnl

	id_val_constructor(`$1',`$2') :: (id_class_hasbundle(`$1') m) => arg_list(`$2') -> m id_type(`$1');
	id_val_constructor(`$1',`$2') args = id_val_getbundle(`$1') >>= (\bundle -> id_val_getfrombundle_constructor(`$1',`$2') bundle args);
divert`'dnl
	id_val_load_constructor(`$1',`$2') :: (TypedLayer.IsJVMMonad m,TypedLayer.IsJVMMonad mm) =>
		TypedLayer.JClass ->
		m (arg_list(`$2') -> mm id_type(`$1'));
	id_val_load_constructor(`$1',`$2') classRef = TypedLayer.getMakeNewObject classRef;
')dnl
dnl
define(`sc_staticfield',`
divert(1)dnl
		,id_val_getfrombundle_staticfield(`$1',`$2')	:: Ref m id_type(`$3')
divert(2)dnl
			id_val_staticfield(`$1',`$2') <- id_val_load_staticfield(`$1',`$2') theClass;
divert(3)dnl
				id_val_staticfield(`$1',`$2')
divert(4)dnl

	id_val_staticfield(`$1',`$2') :: (id_class_hasbundle(`$1') m) => Ref m id_type(`$3');
	id_val_staticfield(`$1',`$2') = utilConvertRef id_val_getbundle(`$1') id_val_getfrombundle_staticfield(`$1',`$2');
divert`'dnl
	id_val_load_staticfield(`$1',`$2',`$3') :: (TypedLayer.IsJVMMonad m,TypedLayer.IsJVMMonad mm) =>
		TypedLayer.JClass ->
		m (Ref mm id_type(`$3'));
	id_val_load_staticfield(`$1',`$2',`$3') classRef = TypedLayer.getStaticField classRef "`$2'";
')dnl
dnl
define(`sc_field',`
divert(1)dnl
		,id_val_getfrombundle_field(`$1',`$2')	:: id_type_short(`$1') -> Ref m id_type(`$3')
divert(2)dnl
			id_val_field(`$1',`$2') <- id_val_load_field(`$1',`$2') theClass;
divert(3)dnl
				id_val_field(`$1',`$2')
divert(4)dnl

	id_val_field(`$1',`$2') :: (id_class_hasbundle(`$1') m,id_class_IsType(`$1') c) => c -> Ref m id_type(`$3');
	id_val_field(`$1',`$2') obj = utilConvertRef id_val_getbundle(`$1') (\bundle -> id_val_getfrombundle_field(`$1',`$2') bundle (id_val_toType(`$1') obj));
divert`'dnl
	id_val_load_field(`$1',`$2') :: (TypedLayer.IsJVMMonad m,TypedLayer.IsJVMMonad mm) =>
		TypedLayer.JClass ->
		m (id_type_class(`$1') -> Ref mm id_type(`$3'));
	id_val_load_field(`$1',`$2') classRef = TypedLayer.getField classRef "`$2'";
')dnl
define(`sc_endclass',`dnl

	data (TypedLayer.IsJVMMonad m) => id_type_bundle(`$1') m = id_cons_bundle(`$1')
		{
		id_val_getfrombundle_class(`$1')	:: TypedLayer.JClass
undivert(1)dnl
		};

	instance (TypedLayer.IsJVMMonad m,TypedLayer.IsJVMMonad mm) => JVMLayer.IsJVMLoadable m (id_type_bundle(`$1') mm) where
		{
		JVMLayer.jvmLoad = do
			{
			theClass <- id_val_load_class(`$1');
undivert(2)dnl
			return (id_cons_bundle(`$1') theClass
undivert(3)dnl
				);
			};
		};

	class (TypedLayer.IsJVMMonad m) => id_class_hasbundle(`$1') m where
		{
		id_val_getbundle(`$1') :: m (id_type_bundle(`$1') m);
		};

	id_val_class(`$1') :: (id_class_hasbundle(`$1') m) => m TypedLayer.JClass;
	id_val_class(`$1') = id_val_getbundle(`$1') >>= (return . id_val_getfrombundle_class(`$1'));
undivert(4)dnl

')dnl
dnl
-- This file was autogenerated from LibPackage.m4.
module id_module(CLASSESFILE) where
	{
	import LibClasses;
	import LibUtil;
	import qualified JVMLayer;
	import qualified TypedLayer;
	import JavaLayer;
	import BasicLayer;
	
include(CLASSESFILE)
	}
