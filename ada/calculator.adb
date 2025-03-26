with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Float_Text_IO;               use Ada.Float_Text_IO;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;   use Ada.Strings.Unbounded.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions;

procedure Calculator is
   type Expr_Kind is (Add, Sub, Mul, Div, Pow, Number, Parens, Variable);
   type Expr;
   type Expr_Access is access Expr;

   type Expr is record
      Kind     : Expr_Kind;
      Left     : Expr_Access := null;
      Right    : Expr_Access := null;
      Value    : Float := 0.0;
      Var_Name : Unbounded_String := To_Unbounded_String("");
   end record;

   package Var_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Float,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Variables : Var_Map.Map;

   Input : Unbounded_String;
   Index : Natural := 1;
   Parse_Success : Boolean := True;

   function Trim(S : String) return String is
      use Ada.Strings.Fixed;
   begin
      return Trim(S, Ada.Strings.Both);
   end;

   function Current_Char return Character is
   begin
      if Index > Length(Input) then
         return ASCII.NUL;
      else
         return Element(Input, Index);
      end if;
   end;

   procedure Advance is
   begin
      Index := Index + 1;
   end;

   procedure Skip_Whitespace is
   begin
      while Current_Char in ' ' | ASCII.HT loop
         Advance;
      end loop;
   end;

   function Parse_Number return Expr_Access is
      Start : Natural := Index;
   begin
      while Current_Char in '0' .. '9' loop
         Advance;
      end loop;

      if Current_Char = '.' then
         Advance;
         while Current_Char in '0' .. '9' loop
            Advance;
         end loop;
      end if;

      declare
         Num_Str : String := To_String(Input)(Start .. Index - 1);
         Num     : Float := Float'Value(Num_Str);
      begin
         return new Expr'(Kind => Number, Left => null, Right => null, Value => Num, Var_Name => To_Unbounded_String(""));
      exception
         when others =>
            Put_Line("Error: invalid number format '" & Num_Str & "'");
            Parse_Success := False;
            return new Expr'(Kind => Number, Left => null, Right => null, Value => 0.0, Var_Name => To_Unbounded_String(""));
      end;
   end;

   function Parse_Identifier return Expr_Access is
      Start : Natural := Index;
   begin
      while Current_Char in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' loop
         Advance;
      end loop;

      declare
         Name : String := To_String(Input)(Start .. Index - 1);
      begin
         return new Expr'(Kind => Variable, Left => null, Right => null, Value => 0.0, Var_Name => To_Unbounded_String(Trim(Name)));
      end;
   end;

   function Parse_Factor return Expr_Access;

   function Parse_Exponent return Expr_Access is
      LHS : Expr_Access := Parse_Factor;
   begin
      Skip_Whitespace;
      if Current_Char = '^' then
         Advance;
         LHS := new Expr'(Kind => Pow, Left => LHS, Right => Parse_Exponent, Value => 0.0, Var_Name => To_Unbounded_String(""));
      end if;
      return LHS;
   end;

   function Parse_Term return Expr_Access is
      LHS : Expr_Access := Parse_Exponent;
   begin
      loop
         Skip_Whitespace;
         case Current_Char is
            when '*' =>
               Advance;
               LHS := new Expr'(Kind => Mul, Left => LHS, Right => Parse_Exponent, Value => 0.0, Var_Name => To_Unbounded_String(""));
            when '/' =>
               Advance;
               LHS := new Expr'(Kind => Div, Left => LHS, Right => Parse_Exponent, Value => 0.0, Var_Name => To_Unbounded_String(""));
            when others =>
               exit;
         end case;
      end loop;
      return LHS;
   end;

   function Parse_Expr return Expr_Access is
      LHS : Expr_Access := Parse_Term;
   begin
      loop
         Skip_Whitespace;
         case Current_Char is
            when '+' =>
               Advance;
               LHS := new Expr'(Kind => Add, Left => LHS, Right => Parse_Term, Value => 0.0, Var_Name => To_Unbounded_String(""));
            when '-' =>
               Advance;
               LHS := new Expr'(Kind => Sub, Left => LHS, Right => Parse_Term, Value => 0.0, Var_Name => To_Unbounded_String(""));
            when others =>
               exit;
         end case;
      end loop;
      return LHS;
   end;

   function Parse_Factor return Expr_Access is
   begin
      Skip_Whitespace;
      if Current_Char = '(' then
         Advance;
         declare
            SubExpr : Expr_Access := Parse_Expr;
         begin
            if Current_Char = ')' then
               Advance;
            else
               Put_Line("Error: expected ')'");
               Parse_Success := False;
            end if;
            return SubExpr;
         end;
      elsif Current_Char in '0' .. '9' then
         return Parse_Number;
      elsif Current_Char in 'a' .. 'z' | 'A' .. 'Z' then
         return Parse_Identifier;
      else
         Put_Line("Error: Unexpected character '" & String'(1 => Current_Char) & "'");
         Parse_Success := False;
         return new Expr'(Kind => Number, Left => null, Right => null, Value => 0.0, Var_Name => To_Unbounded_String(""));
      end if;
   end;

   function Evaluate(E : Expr_Access; Success : out Boolean) return Float is
   begin
      Success := True;
      case E.Kind is
         when Number =>
            return E.Value;
         when Variable =>
            declare
               Name : constant String := Trim(To_String(E.Var_Name));
            begin
               if Variables.Contains(Name) then
                  return Variables.Element(Name);
               else
                  Put_Line("Error: unknown variable '" & Name & "'");
                  Success := False;
                  return 0.0;
               end if;
            end;
         when Pow =>
            declare
               Ls, Rs : Boolean;
               Base  : Float := Evaluate(E.Left, Ls);
               Expo  : Float := Evaluate(E.Right, Rs);
            begin
               Success := Ls and Rs;
               return Ada.Numerics.Elementary_Functions."**"(Base, Expo);
            end;
         when Add | Sub | Mul | Div =>
            declare
               Ls, Rs : Boolean;
               Lv  : Float := Evaluate(E.Left, Ls);
               Rv  : Float := Evaluate(E.Right, Rs);
            begin
               Success := Ls and Rs;
               if not Success then
                  return 0.0;
               end if;

               case E.Kind is
                  when Add => return Lv + Rv;
                  when Sub => return Lv - Rv;
                  when Mul => return Lv * Rv;
                  when Div =>
                     if Rv = 0.0 then
                        Put_Line("Error: Division by zero.");
                        Success := False;
                        return 0.0;
                     else
                        return Lv / Rv;
                     end if;
                  when others => return 0.0;
               end case;
            end;
         when Parens =>
            return Evaluate(E.Left, Success);
      end case;
   end;

begin
   Put_Line("ADA Calculator (with variables) — Type 'q' to quit.");
   loop
      Put(">> ");
      Input := To_Unbounded_String(Get_Line);
      declare
         Line       : constant String := To_String(Input);
         Pos        : Natural := Ada.Strings.Fixed.Index(Line, ":=");
         Op_Length  : Natural := 2;
      begin
         exit when Trim(Line) = "q";

         if Pos = 0 then
            Pos := Ada.Strings.Fixed.Index(Line, "=");
            Op_Length := 1;
         end if;

         if Pos > 0 then
            declare
               Var_Name : constant String := Trim(Line(1 .. Pos - 1));
               Expr_Str : constant String := Trim(Line(Pos + Op_Length .. Line'Last));
            begin
               Input := To_Unbounded_String(Expr_Str);
               Index := 1;
               Parse_Success := True;
               declare
                  AST     : Expr_Access;
                  Result  : Float;
                  Eval_Success : Boolean;
               begin
                  AST := Parse_Expr;
                  Result := Evaluate(AST, Eval_Success);
                  if Parse_Success and Eval_Success then
                     Variables.Include(Trim(Var_Name), Result);
                     Put(Var_Name & " = ");
                     if Float'Floor(Result) = Result then
                        Put_Line(Integer'Image(Integer(Result)));
                     else
                        Put(Result, Fore => 1, Aft => 6, Exp => 0);
                        New_Line;
                     end if;
                  end if;
               end;
            end;
         else
            Index := 1;
            Parse_Success := True;
            declare
               AST     : Expr_Access;
               Result  : Float;
               Eval_Success : Boolean;
            begin
               AST := Parse_Expr;
               Result := Evaluate(AST, Eval_Success);
               if Parse_Success and Eval_Success then
                  if Float'Floor(Result) = Result then
                     Put_Line("= " & Integer'Image(Integer(Result)));
                  else
                     Put("= ");
                     Put(Result, Fore => 1, Aft => 6, Exp => 0);
                     New_Line;
                  end if;
               end if;
            end;
         end if;
      end;
   end loop;
end Calculator;
