with Ada.Exceptions;

package Exceptions is

   -- This exception should be used when a verb expects a stack item of a
   -- given type, and gets something else.
   Bad_Type_Exception: exception;

   -- This is for "should never happen".  We all know it will ;-).
   Interpreter_Bug: exception;

   -- Self-explanatory.
   Stack_Underflow: exception;

   -- This is when an item that isn't a string has ended up on an
   -- execution stack.
   Bad_Exec_Token: exception;

   -- This is when a token is syntactically (morphologically?) bad.
   Bad_Token: exception;

   -- When a string is used as an integer and it isn't one...
   Bad_Implicit_Conversion: exception;

   -- When a stack that doesn't exist is used.  This will only occur in the root
   -- context, when there are no input and output stacks defined.
   No_Such_Stack: exception;

   -- Self-explanatory
   No_Such_Verb: exception;
   Test_Failed: exception;
   Program_Died: exception;
   No_Loadable_Module_Support: exception;

   -- A shared exception handler
   procedure Handle_TISBL_Error_Or_Reraise(E: Ada.Exceptions.Exception_Occurrence);

end Exceptions;
