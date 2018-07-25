
# Darts Tools

This is a small library of more or less useful stuff. Most things in
here used to be carried over from project to project via copy+paste.

## Documentation

### Generic Property List Support

Sometimes it is handy, if an application can store additional data
in objects managed by other (say: library) code. One way to achieve
that is to add support for "property lists" to objects. 

This library provides a generic mechanism for that. The basis are
the following generic functions, which form the "core" of the 
property support:

 - **Generic Function** `property-list` _object_ &rarr; _plist_
 
   Answers a list of keyword/value pairs in plist format, i.e., 
   of the form `(indicator1 value1 indicator2 value2 ...)`, which
   holds the properties associated with _object_. The result
   should be considered immutable by the caller and must not be
   destructively modified.
   
 - **Generic Function** `update-property-list` _object_ _modifier_ &rarr; _result_
 
   Modifies the property list associated with _object_ by invoking
   the given _modifier_ function on the current list, and storing
   the result as new property list. The _modifier_ is not allowed
   to destructively modify its input argument; it may, however,
   produce a new property list, that shares parts of its structure
   with the old one.
   
   The _modifier_ must be a function, that returns two values. The
   first one is the new property list to associate with _object_.
   The second value is arbitrary and will be returned from 
   `update-property-list`.
   
   In lisp implementations, where it can be easily implemented, the
   update process happens atomically. Note, that in such an
   implementation, the _modifier_ function can be called multiple
   times. For this reason, side-effects should be avoided.

Given an object of a type, which supports these functions, the following
accessors and modifiers work out-of-the-box.

 - **Function** `property-value` _object_ _indicator_ `&optional` _default &rarr; _value_ _foundp_
 
   Looks up the property with the given _indicator_ (a symbol) in
   the property list associated with _object_. If the property is
   found, its value is returned as the primary result _value_, and
   the secondary result is true. If the property has not been 
   found, the value of _default_ is returned instead as the _value_,
   and the _foundp_ flag will be false. The default value for _default_
   itself is `nil`.
   
The `property-value` function is `setf`-able, provided, that a suitable
method on `update-property-list` exists for the argument object:

 - **Function** `(setf property-value)` _new-value_ _object_ _indicator_ `&optional` _default_ &rarr; _new-value_
 
   Note, that _default_ is ignored here.

Besides using `setf`, an application can modify object property lists
by various other functions provided here.

 - **Function** `ensure-property` _object_ _indicator_ _constructor_ &rarr; _foundp_ _stored-value_
 
   Ensures, that there is a property named _indicator_ associated
   with _object_. If such a property already exists in the property
   list, this function does nothing. Otherwise, it invokes the
   given _constructor_ function, and stores whatever that function
   returns, as the value of the property in the object's plist.
   
   The constructor function should not have any side-effects, as
   it can be invoked multiple times in a row in lisp implementations,
   where `update-property-list` performs the updates atomically.
   
   This function returns as primary value _foundp_ a flag, which
   indicates, whether the property did already exists (true), or 
   not (false). The second value _stored-value_ is the property 
   value associated with the given _indicator_ after the call. If
   the property did already exist, it is the "old" value. If the
   constructor had to be invoked, it is the value obtained from
   it and stored in the new plist.
   
 - **Function** `remove-property` _object_ _indicator_ &rarr; _value_ _foundp_
 
   Removes the property named by _indicator_ from the property list
   of _object_. The primary result _value_ is the form value 
   associated with the property (`nil`, if the property was not
   found). The secondary value _foundp_ is a boolean flag, which
   is true, if the property was found (and removed), and false,
   if no matching property exists.
 
 - **Function** `remove-properties` _object_ `&optional` _which_ &rarr; _removed_
 
   Removes all properties from the property associated with
   _object_, which match the value of _which_. The following
   values are supported for the _which_ argument:
   
    - `t` instructs the function to remove *all* properties
    - a list of indicators (symbols) cause the function to
      remove all properties, whose indicators are listed here
      
   The default value for _which_ is `t`, causing all properties
   to be removed.
      
   This function returns another plist, which contains all 
   the key/value pairs removed.
   
 - **Function** `filter-properties` _predicate_ _object_ &rarr; _removed_
 
   This is a generalized version of `remove-properties`, which
   removes all those entries, that match _predicate_. The value
   of _predicate_ must be a function of two arguments. The first
   one is the property indicator, and the second one the associated
   value. If the function returns true, the property kept, otherwise
   it is removed from the property list.

   This function returns another plist, which contains all 
   the key/value pairs removed.

### Iteration

 - **Function** `map-over-properties` _function_ _object_ &rarr; _object_
 
   Invokes _function_ for all key/value pairs in the property
   list associated with _object_. The function is called with
   the indicator as first, and the value as second argument, and
   its return value is ignored.
   
   `Map-over-properties` returns the value of the _object_ 
   argument.

 - **Macro** `do-properties` `(` _key_ _value_ `)` _object_ `&body` _body_
 
   Evaluates the _object_ form, and obtains the property list of
   the resulting value. Then introduces new bindings for the names
   supplied as _key_ and _value_. For each key/value pair in that 
   property list, sets _key_ to the pair's key and _value_ to the
   associated value and evaluates all forms in _body_ sequentially.
   
   The body of a `do-properties` form is an implicit `tagbody`. Also,
   this macro establishes an anonymous block around its expansion.
   Unless _body_ establishes a result value by `return`ing from
   that block, the result of the `do-properties` form is `nil`.
   
### DEFSTRUCT and CLOS Support

This library provides a simple mixin class, which can be added to
an application's class hierarchy in order to gain automatic support
for property lists.

 - **Class** `property-support` 
 
   By mixing this class into a class hierarchy, all instances gain
   support for `property-list` and `update-property-list`, and thus,
   for all the accessor and modifier functions defined in here,
   which are built on top of these functions.
   
   The property list is stored in a slot named `property-list`.
   
   Applications *should never* manipulate the contents of this
   slot, as it may not directly contain the property list, but 
   may instead hold some kind of wrapper (maybe even a structure
   instance).
   
 - **Initarg** `property-list` _list_
 
   This initarg can be used with all instances of `property-support`
   to initialize the instance's property list. The name is intentionally
   not a keyword.
   
   Note, that support for this initarg is added by a method on
   `shared-initialize`, not in the slot declaration. The value supplied 
   as _list_ is always (shallowly) copied.

Example:

```
(defclass node (property-support) ())

(defclass parent-node (node)
  ((children :initform nil)))
  
(defclass child-node (node)
  ((parent :initform nil)))
  
(defclass inner-node (parent-node child-node) 
  ())
  
(defvar *node* (make-instance 'inner-node 'property-list (list 'id 16 'display-name "Folder")))

(property-value *node* 'id) ;; => 16
(property-value *node* 'display-name) ;; => "Folder"
```

To add property lists to an application defined structure type, you
have to

 - add a slot of type `list` to the structure type (it usually 
   should have an initial value of `()` and not be `read-only`)
   
 - use the `define-structure-property-list` "declaration" to
   derive property support for your structure type.
   
Example:

```
(defstruct (handle (:copier nil) 
                   (:constructor make-handle (value &optional plist-init 
                                              &aux (plist (copy-list plist-init)))))
  (value 0 :type fixnum :read-only t)
  (plist nil :type list))
  
(define-structure-property-list handle handle-plist)
```

### Missing Support for `SYMBOL-PLIST`

This module does *not* map `PROPERTY-LIST` to `SYMBOL-PLIST` for
symbols, though doing so seems be trivial. However, the way atomic
property updates work in some implementations is not easily adaptable
to what needs to be stored in a symbol's plist slot. Since I rarely
use property lists of symbols (somewhat funny, as I use them for
other stuff all the time), I decided to not open that can of worms
right now.

