(in-package #:org.shirakumo.atomics)

(docs:define-docs
  (type implementation-not-supported
    "Error signalled on unsupported implementations.

This error may be signalled when the library is loaded and the current
implementation is entirely unsupported, or if the current operation is
not supported by the implementation.

See OPERATION")

  (function operation
    "Returns the unsupported operation or NIL on entirely unsupported implementations.

See IMPLEMENTATION-NOT-SUPPORTED")
  
  (function cas
    "Perform a Compare-And-Swap.

Returns non-NIL if successful, NIL if not.
On success it is guaranteed that the PLACE has been set to the NEW
value atomically, that is to say while the PLACE still had a value EQ
to OLD. On failure the value of PLACE has most likely changed to one
not EQ to OLD.

Restrictions apply to allowed PLACEs by implementation:

             Allegro CCL Clasp ECL LispWorks Mezzano SBCL CMUCL
CAR             X          X    X     X         X     X     X  
CDR             X          X    X     X         X     X     X  
FIRST                      X    X               X     X     X  
REST                       X    X               X     X     X  
SVREF           X     X    X    X     X         X     X     X  
SYMBOL-PLIST    X          X    X                     X     X  
SYMBOL-VALUE    X          X    X     X         X     X     X  
SLOT-VALUE      X*         X    X*    X*        X     X*    X  
MEMREF          X                                     X*        
MEMREF-INT      X                                     X*        
struct-slot     X     X*   X    X*    X         X     X*    X  
special-var     X     X    X    X     X         X     X     X  
custom                     X    X     X         X     X     X  

For each of the above feature flags are pushed when available:

  :ATOMICS-CAS-CAR
  :ATOMICS-CAS-SVREF
  :ATOMICS-CAS-SYMBOL-PLIST
  :ATOMICS-CAS-SYMBOL-VALUE
  :ATOMICS-CAS-SLOT-VALUE
  :ATOMICS-CAS-MEMREF
  :ATOMICS-CAS-STRUCT-SLOT
  :ATOMICS-CAS-SPECIAL-VAR
  :ATOMICS-CAS-CUSTOM

Further restrictions apply:

# Allegro                               (EXCL:ATOMIC-CONDITIONAL-SETF)
- SLOT-VALUE can only be used for :INSTANCE and :CLASS allocated slots
  and the SLOT-VALUE-USING-CLASS protocol is ignored.
- struct-slot definitions must be available at compile-time.
- SYMBOL-VALUE accesses the dynamically bound value if it is
  dynamically bound, rather than the global value.

# CCL                                         (CCL::CONDITIONAL-STORE)
- struct-slot accesses only seem to work quite right for T-typed
  slots.

# Clasp                                                       (MP:CAS)
- SLOT-VALUE can only be used for :INSTANCE and :CLASS allocated
  slots. Methods on SLOT-VALUE-USING-CLASS etc. are ignored. If the
  slot is missing, SLOT-MISSING is called with operation = MP:CAS,
  and new-value = a list of OLD and NEW. The slot must be bound.
- SYMBOL-VALUE (and a special variable directly) accesses the
  dynamically bound value if it is dynamically bound, rather than the
  global value.

# ECL                                            (MP:COMPARE-AND-SWAP)
- If a slot is unbound, an error is signalled unless the OLD value is
  SI:UNBOUND. If the slot has any methods defined on
  SLOT-VALUE-USING-CLASS, the consequences are undefined.
- struct-slots must be defined with the :atomic-accessors option.
  See DEFSTRUCT for a portable wrapper.

# LispWorks                                  (SYSTEM:COMPARE-AND-SWAP)
- SLOT-VALUE can only be used for :INSTANCE and :CLASS allocated slots
  and will not work with SLOT-VALUE-USING-CLASS.

# Mezzano                        (MEZZANO.EXTENSIONS:COMPARE-AND-SWAP)
- If a slot has methods defined on SLOT-VALUE-USING-CLASS or
  SLOT-BOUNDP-USING-CLASS, then it must also have a method defined
  on (MEZZANO.EXTENSIONS:CAS SLOT-VALUE-USING-CLASS) for CAS of
  SLOT-VALUE to work. If the slot is missing, SLOT-MISSING is called
  with operation = MEZZANO.EXTENSIONS:CAS, and new-value = a list of
  OLD and NEW. If a slot is unbound, an error is signalled unless
  the OLD value is MEZZANO.CLOS:+SLOT-UNBOUND+.

# SBCL                                                    (SB-EXT:CAS)
- If a slot is unbound, an error is signalled unless the OLD value is
  SB-PCL:+SLOT-UNBOUND+. If the slot has methods defined on
  SLOT-VALUE-USING-CLASS or SLOT-BOUNDP-USING-CLASS, the consequences
  are undefined.
- struct-slot types must be either FIXNUM or T.
- CAS on SB-SYS:SAP-REF-* only since version 2.5.1

# CMUCL                               (based on MP:WITHOUT-SCHEDULING)
- On CMUCL green threads are implemented on X86 ports only
- Due to nature of threads atomic operations are blocking.")
  
  (function atomic-incf
    "Atomically increases place by the specified delta.

Returns the value the place has been set to.

Restrictions apply to allowed PLACEs by implementation:

             Allegro CCL Clasp ECL LispWorks Mezzano SBCL CMUCL
CAR             X     X    X    X     X         X     X     X  
CDR             X     X    X    X     X         X     X     X  
FIRST                      X    X               X     X     X  
REST                       X    X               X     X     X  
SVREF           X     X    X    X     X         X           X  
AREF                                            X     X     X  
SYMBOL-VALUE    X          X    X     X         X           X  
SLOT-VALUE      X*         X    X*    X*        X           X  
MEMREF          X                                              
MEMREF-INT      X                                              
struct-slot     X     X*   X    X*    X         X     X*    X  
special-var     X     X    X    X     X         X           X  
custom                     X    X     X         X     X     X  
global (SBCL)                                         X*       

Further restrictions apply:

# Allegro                                           (EXCL:ATOMIC-INCF)
See CAS

# Clasp                                               (MP:ATOMIC-INCF)
See CAS

# CCL                                           (CCL:ATOMIC-INCF-DECF)
See CAS

# ECL                                                 (MP:ATOMIC-INCF)
See CAS
- The places must store a FIXNUM.
- The addition is performed with modular arithmetic, meaning over- or
  underflows will wrap around.

# LispWorks                                       (SYSTEM:ATOMIC-INCF)
See CAS

# Mezzano                             (MEZZANO.EXTENSIONS:ATOMIC-INCF)
See CAS
- The places must store a FIXNUM.
- The addition is performed with modular arithmetic, meaning over- or
  underflows will wrap around.

# SBCL                                            (SB-EXT:ATOMIC-INCF)
See CAS
- struct-slots must be of type SB-EXT:WORD.
- AREF only works on (SIMPLE-ARRAY SB-EXT:WORD (*)).
- Other places must be a FIXNUM.
- The addition is performed with modular arithmetic, meaning over- or
  underflows will wrap around.

# CMUCL
See CAS")

  (function atomic-decf
    "Atomically decreases place by the specified delta.

Returns the value the place has been set to.

For restrictions, see ATOMIC-INCF.

See ATOMIC-INCF")

  (function atomic-pop
    "Atomically pops place, much like CL:POP.

Returns the value which has been popped from place.

For restrictions, see ATOMIC-PUSH.

See ATOMIC-PUSH")

  (function atomic-push
    "Atomically pushes value into place, much like CL:PUSH.

Returns the value the place has been set to.

In Allegro, ECL, LispWorks, and SBCL, their native implementation of atomic pop and push are used.
On implementations that do not provide such operations, a fallback based on ATOMIC-CAS is available for usage.

Restrictions apply to allowed PLACEs by implementation, see CAS.

See CAS")

  (function atomic-update
    "Updates the PLACE with the value retrieved from UPDATE-FN

The UPDATE-FN is called with the old value of the PLACE and should
return the new value to set the place to.
The PLACE and UPDATE-FN may be evaluated multiple times.

Restrictions apply to the PLACE, see CAS.

See CAS")
  
  (function defstruct
    "Wrapper around CL:DEFSTRUCT to portably define atomically modifiable structures.

See CL:DEFSTRUCT"))
