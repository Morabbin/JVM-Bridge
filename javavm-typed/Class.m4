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
define(`zeroval',`define(`curval',0)')dnl
define(`incrval',`define(`curval',incr(curval))')dnl
define(`first_lower',`regexp(`$1',`^\(.\)\(.*\)',`translit(`\1',`A-Z',`a-z')`\2'')')dnl
define(`jvm_classname',`patsubst(`$1',`\.',`\/')')dnl
define(`name_short',`patsubst(patsubst(`$1',`^.*\.\([^.]+\)$',`\1'),`\$',`_')')dnl
define(`name_long',`patsubst(patsubst(`$1',`_',`__'),`[\.\$]',`_')')dnl
define(`first_J',`patsubst(`$1',`[A-Za-z0-9_.]+',`J`'\&')')dnl
define(`id_module',`Class_`'name_long(`$1')')dnl
define(`id_type_class',`first_J(name_long(`$1'))')dnl
define(`id_type_short',`first_J(name_short(`$1'))')dnl
define(`id_type_marker',`Class_`'id_type_class(`$1')')dnl
define(`id_type_primitive',`first_J(`$1')')dnl
define(`id_type',`ifelse(regexp(`$1',`\.'),`-1',id_type_primitive(`$1'),id_type_class(`$1'))')dnl
define(`is_primitive',`ifelse(regexp(`$1',`^[a-z]+$'),`-1',`$3',`$2')')dnl
define(`val_type',`patsubst(patsubst(patsubst(`$1',`[A-Za-z0-9_.$]+',`id_type(`\&')'),`\[',``(TypedLayer.ArrayRef ''),`\]',``)'')')dnl
define(`isa_type',`incrval`'is_primitive(`$1',`id_type_primitive(`$1')',`a`'curval`'')')dnl
define(`name_type',`incrval`'a`'curval`'')dnl
define(`cname_type',`incrval`'is_primitive(`$1',`a`'curval`'',`ca`'curval`'')')dnl
define(`context_type',`incrval`'is_primitive(`$1',`',`,
		TypedLayer.JVMIsA val_type(`$1') a`'curval`'')')dnl
define(`convert_type',`incrval`'is_primitive(`$1',`',`		ca`'curval <- TypedLayer.jvmConvert a`'curval;
')')dnl
define(`arg_list',`patsubst(`$1',`[][A-Za-z0-9_.$]+',`val_type(`\&')')')dnl
define(`isa_arg_list',`zeroval`'patsubst(`$1',`[][A-Za-z0-9_.$]+',`isa_type(`\&')')')dnl
define(`name_arg_list',`zeroval`'patsubst(`$1',`[][A-Za-z0-9_.$]+',`name_type(`\&')')')dnl
define(`cname_arg_list',`zeroval`'patsubst(`$1',`[][A-Za-z0-9_.$]+',`cname_type(`\&')')')dnl
define(`context_arg_list',`zeroval`'patsubst(patsubst(patsubst(patsubst(``$1'',`[()]',`'),`,',`/'),`[][A-Za-z0-9_.$]+',``context_type(`\&')''),`/',`')')dnl
define(`convert_arg_list',`zeroval`'patsubst(patsubst(`$1',`[][A-Za-z0-9_.$]+',`convert_type(`\&')'),`[(),]',`')')dnl
dnl
define(`id_pp_type',`ifelse(regexp(`$1',`\.'),`-1',id_type_primitive(`$1'),id_type_short(`$1'))')dnl
dnl define(`id_pp_array',`patsubst(`$1',`\[\([^\[\]]+\)\]',`Array`'id_pp_array(`\&')')')dnl
define(`id_pp_array',`patsubst(patsubst(`$1',`\[',`Array'),`\]',`')')dnl
define(`id_pp_arg_list',`patsubst(patsubst(patsubst(id_pp_array(patsubst(`$1',`[A-Za-z0-9_.$]+',`id_pp_type(`\&')')),`[,]',`_'),`[()]',`'),`^.',`_\&')')dnl
dnl
define(`id_pp_field',``$2'_`'id_type_short(`$1')`'')dnl
dnl define(`id_pp_field',``$2'')dnl
define(`id_pp_method',`id_pp_field(`$1',`$2')`'id_pp_arg_list(`$3')')dnl
define(`id_part_class',`Class_`'id_type_short(`$1')')dnl
define(`id_part_method',`Method_`'id_pp_method(`$1',`$2',`$3')')dnl
define(`id_part_staticmethod',`StaticMethod_`'id_pp_method(`$1',`$2',`$3')')dnl
define(`id_part_field',`Field_`'id_pp_field(`$1',`$2')')dnl
define(`id_part_staticfield',`StaticField_`'id_pp_field(`$1',`$2')')dnl
define(`id_part_constructor',`New_`'id_type_short(`$1')`'id_pp_arg_list(`$2')')dnl
dnl
define(`id_val_load_class',`load`'id_part_class(`$1')')dnl
define(`id_val_load_field',`load`'id_part_field(`$1',`$2')')dnl
define(`id_val_load_staticfield',`load`'id_part_staticfield(`$1',`$2')')dnl
define(`id_val_load_method',`load`'id_part_method(`$1',`$2',`$3')')dnl
define(`id_val_load_staticmethod',`load`'id_part_staticmethod(`$1',`$2',`$3')')dnl
define(`id_val_load_constructor',`load`'id_part_constructor(`$1',`$2')')dnl
define(`id_val_load_field',`load`'id_part_field(`$1',`$2')')dnl
define(`id_val_load_staticfield',`load`'id_part_staticfield(`$1',`$2')')dnl
dnl
define(`id_type_preload',`Preload_`'name_short(`$1')')dnl
define(`id_val_preload',`?preload_`'name_long(`$1')')dnl
define(`id_cons_preload',`Mk`'id_type_preload(`$1')')dnl
define(`id_val_getfrompreload_class',`preload`'id_part_class(`$1')')dnl
define(`id_val_getfrompreload_method',`preload`'id_part_method(`$1',`$2',`$3')')dnl
define(`id_val_getfrompreload_staticmethod',`preload`'id_part_staticmethod(`$1',`$2',`$3')')dnl
define(`id_val_getfrompreload_constructor',`preload`'id_part_constructor(`$1',`$2')')dnl
define(`id_val_getfrompreload_field',`preload`'id_part_field(`$1',`$2')')dnl
define(`id_val_getfrompreload_staticfield',`preload`'id_part_staticfield(`$1',`$2')')dnl
define(`context_preload',`id_val_preload(`$1') :: id_type_preload(`$1')')dnl
dnl
define(`id_val_class',`class_`'id_type_short(`$1')')dnl
define(`id_val_method',`first_lower(id_pp_method(`$1',`$2',`$3'))')dnl
define(`id_val_staticmethod',`first_lower(id_pp_method(`$1',`$2',`$3'))')dnl
define(`id_val_constructor',`first_lower(id_pp_method(`$1',`new',`$2'))')dnl
define(`id_val_field',`first_lower(id_pp_field(`$1',`$2'))')dnl
define(`id_val_staticfield',`first_lower(id_pp_field(`$1',`$2'))')dnl
dnl
define(`mkv',`v_`$1'')dnl
define(`id_v_method',`mkv(id_pp_method(`$1',`$2',`$3'))')dnl
define(`id_v_staticmethod',`mkv(id_pp_method(`$1',`$2',`$3'))')dnl
define(`id_v_constructor',`mkv(id_pp_method(`$1',`new',`$2'))')dnl
define(`id_v_field',`mkv(id_pp_field(`$1',`$2'))')dnl
define(`id_v_staticfield',`mkv(id_pp_field(`$1',`$2'))')dnl
dnl
define(`sc_subclass',`	instance TypedLayer.SubJavaClassMarker id_type_marker(`$2')	id_type_marker(`$1');')dnl
dnl
define(`sc_classname',`

	-- `$1'
ifelse(HEADER,`',`',`
	data id_type_marker($1);

	instance TypedLayer.IsJavaClassMarker id_type_marker($1) where
		{
		cName Type = "jvm_classname($1)";
		};

	type id_type_class($1) = TypedLayer.ObjectRef id_type_marker($1);
')dnl
ifelse(id_type_short(`$1'),id_type_class(`$1'),`',`
	type id_type_short(`$1') = id_type_class(`$1');
')
	id_val_load_class(`$1') :: TypedLayer.VM TypedLayer.JClass;
	id_val_load_class(`$1') = TypedLayer.findClass (Type :: Type id_type_short(`$1'));

sc_subclass(`$1',`$1')')dnl
dnl
define(`sc_staticmethod',`
divert(1)dnl
		,id_val_getfrompreload_staticmethod(`$1',`$2',`$4')	:: arg_list(`$4') -> TypedLayer.VM val_type(`$3')
divert(2)dnl
			id_v_staticmethod(`$1',`$2',`$4') <- id_val_load_staticmethod(`$1',`$2',`$4') theClass;
divert(3)dnl
				id_v_staticmethod(`$1',`$2',`$4')
divert(4)dnl

	id_val_staticmethod(`$1',`$2',`$4') ::
		(
		context_preload(`$1')`'context_arg_list(`$4')
		) =>
	 isa_arg_list(`$4') -> TypedLayer.VM val_type(`$3');
	id_val_staticmethod(`$1',`$2',`$4') name_arg_list(`$4') = do
		{
convert_arg_list(`$4')dnl
		id_val_getfrompreload_staticmethod(`$1',`$2',`$4')
		 id_val_preload(`$1') cname_arg_list(`$4');
		};
divert`'dnl
	id_val_load_staticmethod(`$1',`$2',`$4') ::
		TypedLayer.JClass ->
		TypedLayer.VM (arg_list(`$4') -> IO val_type(`$3'));
	id_val_load_staticmethod(`$1',`$2',`$4') classRef = TypedLayer.getStaticMethod classRef "`$2'";
')dnl
dnl
define(`sc_method',`
divert(1)dnl
		,id_val_getfrompreload_method(`$1',`$2',`$4')	:: id_type_short(`$1') -> arg_list(`$4') -> TypedLayer.VM val_type(`$3')
divert(2)dnl
			id_v_method(`$1',`$2',`$4') <- id_val_load_method(`$1',`$2',`$4') theClass;
divert(3)dnl
				id_v_method(`$1',`$2',`$4')
divert(4)dnl

	id_val_method(`$1',`$2',`$4') ::
		(
		context_preload(`$1'),
		TypedLayer.JVMIsA id_type_class(`$1') c`'context_arg_list(`$4')
		) =>
	 c -> isa_arg_list(`$4') -> TypedLayer.VM val_type(`$3');
	id_val_method(`$1',`$2',`$4') obj name_arg_list(`$4') = do
		{
		cobj <- TypedLayer.jvmConvert obj;
convert_arg_list(`$4')dnl
		id_val_getfrompreload_method(`$1',`$2',`$4')
		 id_val_preload(`$1') cobj cname_arg_list(`$4');
		};
divert`'dnl
	id_val_load_method(`$1',`$2',`$4') ::
		TypedLayer.JClass ->
		TypedLayer.VM (id_type_class(`$1') -> arg_list(`$4') -> IO val_type(`$3'));
	id_val_load_method(`$1',`$2',`$4') classRef = TypedLayer.getMethod classRef "`$2'";
')dnl
define(`sc_constructor',`
divert(1)dnl
		,id_val_getfrompreload_constructor(`$1',`$2')	:: arg_list(`$2') -> TypedLayer.VM val_type(`$1')
divert(2)dnl
			id_v_constructor(`$1',`$2') <- id_val_load_constructor(`$1',`$2') theClass;
divert(3)dnl
				id_v_constructor(`$1',`$2')
divert(4)dnl

	id_val_constructor(`$1',`$2') ::
		(
		context_preload(`$1')`'context_arg_list(`$2')
		) =>
	 isa_arg_list(`$2') -> TypedLayer.VM val_type(`$1');
	id_val_constructor(`$1',`$2') name_arg_list(`$2') = do
		{
convert_arg_list(`$2')dnl
		id_val_getfrompreload_constructor(`$1',`$2')
		 id_val_preload(`$1') cname_arg_list(`$2');
		};
divert`'dnl
	id_val_load_constructor(`$1',`$2') ::
		TypedLayer.JClass ->
		TypedLayer.VM (arg_list(`$2') -> IO val_type(`$1'));
	id_val_load_constructor(`$1',`$2') classRef = TypedLayer.getMakeNewObject classRef;
')dnl
dnl
define(`sc_staticfield',`
divert(1)dnl
		,id_val_getfrompreload_staticfield(`$1',`$2')	:: Ref IO val_type(`$3')
divert(2)dnl
			id_v_staticfield(`$1',`$2') <- id_val_load_staticfield(`$1',`$2') theClass;
divert(3)dnl
				id_v_staticfield(`$1',`$2')
divert(4)dnl

	id_val_staticfield(`$1',`$2') :: (context_preload(`$1')) => Ref IO val_type(`$3');
	id_val_staticfield(`$1',`$2') = id_val_getfrompreload_staticfield(`$1',`$2') id_val_preload(`$1');
divert`'dnl
	id_val_load_staticfield(`$1',`$2',`$3') ::
		TypedLayer.JClass ->
		TypedLayer.VM (Ref IO val_type(`$3'));
	id_val_load_staticfield(`$1',`$2',`$3') classRef = TypedLayer.getStaticField classRef "`$2'";
')dnl
dnl
define(`sc_field',`
divert(1)dnl
		,id_val_getfrompreload_field(`$1',`$2')	:: id_type_short(`$1') -> Ref IO val_type(`$3')
divert(2)dnl
			id_v_field(`$1',`$2') <- id_val_load_field(`$1',`$2') theClass;
divert(3)dnl
				id_v_field(`$1',`$2')
divert(4)dnl

	id_val_field(`$1',`$2') :: (context_preload(`$1'),IsA id_type_class(`$1') c) => c -> Ref IO val_type(`$3');
	id_val_field(`$1',`$2') obj = id_val_getfrompreload_field(`$1',`$2') id_val_preload(`$1') (upcast obj);
divert`'dnl
	id_val_load_field(`$1',`$2') ::
		TypedLayer.JClass ->
		TypedLayer.VM (id_type_class(`$1') -> Ref IO val_type(`$3'));
	id_val_load_field(`$1',`$2') classRef = TypedLayer.getField classRef "`$2'";
')dnl
define(`sc_endclass',`dnl

	data id_type_preload(`$1') = id_cons_preload(`$1')
		{
		id_val_getfrompreload_class(`$1')	:: TypedLayer.JClass
undivert(1)dnl
		};

	instance TypedLayer.IsJVMLoadable id_type_preload(`$1') where
		{
		jvmLoad = do
			{
			theClass <- id_val_load_class(`$1');
undivert(2)dnl
			return (id_cons_preload(`$1') theClass
undivert(3)dnl
				);
			};
		};

	id_val_class(`$1') :: (context_preload(`$1')) => TypedLayer.JClass;
	id_val_class(`$1') = id_val_getfrompreload_class(`$1') id_val_preload(`$1');
undivert(4)dnl

')dnl
dnl
{-# LANGUAGE UndecidableInstances, OverloadedStrings #-}
{-# OPTIONS -Wall -Werror -fno-warn-unused-imports -fno-warn-orphans #-}
-- This file was autogenerated from Class.m4.
module MODULENAME where
	{
patsubst(IMPORTS,`[A-Za-z._0-9]+',`	import \&;
')dnl
	import qualified Foreign.JavaVM.Typed as TypedLayer;
	import Platform.JavaVM;
	import Control.Monad.Reference;
	import Data.Subtype;
	import Data.Witness;
	
include(CLASSESFILE)
	}
