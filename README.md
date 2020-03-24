
# Darts Tools

This is a small library of more or less useful stuff. Most things in
here used to be carried over from project to project via copy+paste.

## Generic Property List Support

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
   
   The update process happens atomically. The _modifier_ function
   must be prepared to call multiple times in case the implementation
   detects a race with some other thread updating the same list 
   concurrently. It should, in particular, not have side-effects.

Given an object of a type, which supports these functions, the following
accessors and modifiers work out-of-the-box.

 - **Function** `property-value` _object_ _indicator_ `&optional` _default_ &rarr; _value_ _foundp_
 
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
   value associated with the given _indicator_ after the call. 
   
   Note, that even if the constructor function is invoked by this
   function, there is no guarantee as to whether its return value
   actually makes it into the "final" version of the property 
   list. The _stored-value_ returned by this function may be
   one installed by a concurrently running thread winning the 
   race.
   
 - **Function** `delete-property` _object_ _indicator_ &rarr; _value_ _foundp_
 
   Removes the property named by _indicator_ from the property list
   of _object_. The primary result _value_ is the form value 
   associated with the property (`nil`, if the property was not
   found). The secondary value _foundp_ is a boolean flag, which
   is true, if the property was found (and removed), and false,
   if no matching property exists.
 
 - **Function** `delete-properties` _object_ `&optional` _which_ &rarr; _removed_
 
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
   
 - **Function** `delete-properties-if-not` _predicate_ _object_ &rarr; _removed_

   This is a generalized version of `remove-properties`, which
   removes all those entries, that do not match _predicate_. The value
   of _predicate_ must be a function of two arguments. The first
   one is the property indicator, and the second one the associated
   value. If the function returns true, the property kept, otherwise
   it is removed from the property list.

   This function returns another plist, which contains all 
   the key/value pairs removed.
 
 - **Function** `delete-properties-if` _predicate_ _object_ &rarr; _removed_
 
   This is a generalized version of `remove-properties`, which
   removes all those entries, that match _predicate_. The value
   of _predicate_ must be a function of two arguments. The first
   one is the property indicator, and the second one the associated
   value. If the function returns true, the property is removed, 
   otherwise it is kept in the property list.

   This function returns another plist, which contains all 
   the key/value pairs removed.

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
   
The following functions exist for compatibility reasons. They should not
be used in new code:

 - **Function** `remove-properties` _object_ `&optional` _which_ &rarr; _removed_
 
   Old name of `delete-properties`. Obsolete.

 - **Function** `remove-property` _object_ _indicator_ &rarr; _value_ _found_
 
   Old name of `delete-property`. Obsolete.

 - **Function** `remove-properties-if` _predicate_ _object_ &rarr; _removed_
 
   Old name of `delete-properties-if`. Obsolete.

 - **Function** `remove-properties-if-not` _predicate_ _object_ &rarr; _removed_
 
   Old name of `delete-properties-if-not`. Obsolete.
   
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

### Atomic Updates

The implementation in this library updates the property lists
atomically. 

**Important** If you want to use the structure property list feature
with ECL, you want to define the structure type with `atomics:defstruct`
instead of `cl:defstruct` in order to get special atomic accessor
support.

### Missing Support for `SYMBOL-PLIST`

This module does *not* map `PROPERTY-LIST` to `SYMBOL-PLIST` for
symbols, though doing so seems be trivial. However, the way atomic
property updates work in some implementations is not easily adaptable
to what needs to be stored in a symbol's plist slot. Since I rarely
use property lists of symbols (somewhat funny, as I use them for
other stuff all the time), I decided to not open that can of worms
right now.

## Iteration Macros

 - **Macro** `named-loop` _name_ `(&rest` _bindings_`)` `&body` _body_
 
   This operator is inspired by Scheme's named-let feature, though less
   general. Each of the _bindings_ is a list of the form `(` _variable_ _form_ `)`.
   This macro binds each of the mentioned names _variable_ to the result
   of evaluating its initializer _form_, and then evaluates the forms 
   of _body_ like `progn`. The values of the last evaluated form are
   returned as the values of the whole operation.
   
   Visible in _body_ is an operator, whose name is _name_, which takes
   as many required arguments as there are binding forms in _bindings_.
   When called, each of the bound variables are get their values
   reassigned from the arguments, and the execution restarts. Note,
   that it is currently unspecified, whether the operator is 
   implemented as (local) function or a macro. Using something like
   `#'`_name_ is an error.
   
   Example:
   
   ```
   (defun my-length (list)
     (named-loop next ((list list) (count 0))
       (if (consp list)
           (next (cdr list) (1+ count))
           count)))
   ```
   
   The whole operator is similar in spirit to the construct
   
   ```
   (labels ((next (list count)
              (if (consp list) 
                  (next (cdr list) (1+ count))
                  count)))
     (next list 0))
   ```
   
   though the expansion is different. This code tries to make sure,
   that tail calls can be eliminated by the compiler.
   
 - **Macro** `label` _name_ `(&rest` _bindings_`)` `&body` _body_
 
## Event Notification

This library provides a simple facility for event notifications. The
feature is split into low-level support code, and a ready to use high 
level mixin-class.

### High-Level API

 - **Generic Function** `add-observer` _observer_ _source_ `&key` _test_ _key_ _identity_ &rarr; _result_ _found_
 
   Add the given _observer_ to _source_'s set of registered
   event observers, unless it is already present.
   
   This function tests for the presence of _observer_ by searching
   for an element _e_ among the observers, whose key value (i.e., 
   the result of applying the _key_ function to _e_) compares equal
   to the _identity_ value according to the _test_ predicate. The
   default _key_ function is `identity`, and the default _test_
   predicate is `eql`. Unless explicitly supplied otherwise, the
   _identity_ value searched for is `(funcall key observer)`.
   
   If the observer is not yet present, it is added. The _result_ is 
   _observer_ in this case, and _found_ is false. If the observer 
   is already present, the value of _result_ is the one found, 
   and _found_ is true.
 
 - **Generic Function** `remove-observer` _observer_ _source_ `&key` _test_ _key_ &rarr; _result_ _found_
 
   Removes the observer object from the set of observers of _source_,
   whose key value (the result of applying _key_ to the object) is 
   equal to _observer_ according to the _test_ predicate. The
   default _key_ function is `identity`, and the default _test_
   predicate is `eql`.
   
   If no matching observer is found, the value of _result_ is `nil`,
   and _found_ is false. Otherwise, the matching entry is removed 
   destructively, and returned as _result_ from this function; 
   _found_ is true in this case.
 
 - **Generic Function** `notify-observers` _source_ _event_ &rarr; _undefined_
 
   Notify all observers registered on _source_, that the event 
   described by _event_ has occurred. This is a nop, if no observers
   have been registered for _source_.
 
 - **Generic Function** `observe-event` _observer_ _source_ _event_ &rarr; _undefined_
  
   Invoked to notify _observer_, that the event described by _event_
   has occurred with respect to object _source_. There is no default
   method. 
 
 - **Class** `observable`
 
   A class, which can be mixed into your application's hierarchy in
   order to provide simple event notifications. Client code can add
   and remove observers using the high-level API to instances of this
   class. Event notifications can be published via `notify-observers`.
 
### Low-Level API

An "observer chain" (or "chain" for brevity here) is a list of lists

    (observers1 observers2 ...)
    
where the elements of each sublist `observersk` are the actual observer
objects. There are two points to this

 - we can update the observer lists in a thread-safe way without having 
   to take a lock via CAS. This is handled automatically by the library.
   
 - it allows us to easily implement nested scopes, where the occurence
   of an event in object `X` should also be propagated to the observers
   registered on `X`'s set of ancestors in some application-defined
   hierarchy.
   
The second use case requires cooperation of your application. Consider
the following example:

```
(defvar *global-chain* (list nil))

(defclass session ()
  ((local-chain :initarg :local-chain)))
  
(defclass transaction ()
  ((local-chain :initarg :local-chain)))
  
(defun start-session ()
  (make-instance 'session :local-chain (cons nil *global-chain*)))
  
(defun begin-transaction (session)
  (make-instance 'transaction 
                  :local-chain (cons nil (slot-value session 'local-chain))))
                  
(defun commit-transaction (transaction &rest keys)
  ;; Do whatever needs to be done...
  (notify-observers-in-chain (slot-value transaction 'local-chain)
                             transaction `(commit :status :success ,@keys)))
```

In this scenario, when a transaction is committed, the event
notification is automatically propagated across the "scopes" of
the hierarchy:

 - first, all observers on the transaction instance itself are notified,
 - then the observers registered for the session, and finally
 - all observers, that had been registered in global scope.
 
 This ordering is guaranteed by the implementation, though the
 order of invocation within each of these scopes is undefined.
 
 - **Function** `add-observer-to-chain` _observer_ _chain_ `&key` _test_ _key_ _identity_ &rarr; _result_ _found_

   Add the given _observer_ to the chain, whose container cell is
   the given _chain_, i.e., to the list stored in _chain_'s `car`.
   If the observer is already present, the chain is not modified.
   
   This function tests for the presence of _observer_ by searching
   for an element _e_ in the observer list, whose key value (i.e., 
   the result of applying the _key_ function to _e_) compares equal
   to the _identity_ value according to the _test_ predicate. The
   default _key_ function is `identity`, and the default _test_
   predicate is `eql`. Unless explicitly supplied otherwise, the
   _identity_ value searched for is `(funcall key observer)`.
   
   If the observer is not yet present, it is added by destructively
   modifying the _chain_. The _result_ is _observer_ in this case,
   and _found_ is false. If the observer is already present, the
   value of _result_ is the one found, and _found_ is true.
   
   Note, that this function will only destructively modify the cons 
   cell _chain_, never the observer list found in the `car`. A 
   copy is created if necessary, which may share structure with the
   original list. This is guaranteed regardless of wether the Lisp
   implementation supports atomic updates or not.

 - **Function** `remove-observer-from-chain` _observer_ _chain_ `&key` _test_ _key_ &rarr; _result_ _found_
 
   Removes the observer object from the chain _chain_, whose
   key value (the result of applying _key_ to the object) is equal
   to _observer_ according to the _test_ predicate. The
   default _key_ function is `identity`, and the default _test_
   predicate is `eql`.
   
   If no matching observer is found, the value of _result_ is `nil`,
   and _found_ is false. The _chain_ is not modified in this case.
   Otherwise, the matching entry is removed destructively, and returned 
   as _result_ from this function; _found_ is true in this case.
   
   Note, that this function will only destructively modify the cons 
   cell _chain_, never the observer list found in the `car`. A 
   copy is created if necessary, which may share structure with the
   original list. This is guaranteed regardless of wether the Lisp
   implementation supports atomic updates or not.
 
 - **Function** `notify-observers-in-chain` _chain_ _source_ _event_ &rarr; _undefined_
 
   Use the standard protocol function `observe-event` to notify 
   each observer in the given _chain_, that the event described by
   _event_ has occurred with respect to object _source_.
 
 - **Macro** `do-observers-in-chain` `(` _observer-var_ `&rest` _bindings_ `)` _chain-form_ `&body` _body_ &rarr; `nil`
 
   Evaluates _chain-form_ first. Loops over all observer objects in
   the resulting chain. For each observer, binds _observer-var_ to that
   object and evaluates the forms in _body_ like `progn`.
 
   If _bindings_ are given, those are also made available during each
   invocation of the body forms. The important point is that the 
   bindings are only established (and their initializer forms are
   only executed) if there is at least one observer in the chain. Also,
   each initializer form is evaluated at most once.
   
   Example
   
   ```
   (do-observers-in-chain (observer 
                           (event-count (count-events-in-database))
                           (summary (compute-event-summary event-count))) some-chain
      (notify-subscribers observer event-count summary))
                                    
   ```
   
   This macro is intended to be used, when the arguments, you need
   to pass to event observers, are costly to compute (which the
   example tries to imply), so you do not want to perform this computation,
   unless you have at least one observer.
   
   Also, this macro allows you to choose any code whatsoever to be
   used as the actual event notification; this allows the use of
   custom generic functions instead of `observe-event` with differing
   parameters. 
 
## Copyright

This library is licensend under the terms of the MIT license:

> Copyright (c) 2019 Dirk Esser
>
> Permission is hereby granted, free of charge, to any person obtaining a copy
> of this software and associated documentation files (the "Software"), to deal
> in the Software without restriction, including without limitation the rights
> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
> copies of the Software, and to permit persons to whom the Software is
> furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in
> all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
> THE SOFTWARE.
