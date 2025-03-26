with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;     use Ada.Strings.Maps;

procedure Calculator is
   type Error_Type is (None, Invalid_Expression, Division_By_Zero);
   
   type Result_Type is record
      Value : Integer;
      Error : Error_Type := None;
      Error_Message : Unbounded_String;
   end record;
   
   function Parse_Expression (Expression : String) return Result_Type;
   
   function Remove_Spaces (Str : String) return String is
      Result : String(1..Str'Length);
      Length : Natural := 0;
   begin
      for I in Str'Range loop
         if Str(I) /= ' ' then
            Length := Length + 1;
            Result(Length) := Str(I);
         end if;
      end loop;
      return Result(1..Length);
   end Remove_Spaces;
   
   function Trim (Str : String) return String is
      Start_Idx : Integer := Str'First;
      End_Idx   : Integer := Str'Last;
   begin
      while Start_Idx <= Str'Last and then Str(Start_Idx) = ' ' loop
         Start_Idx := Start_Idx + 1;
      end loop;
      
      while End_Idx >= Str'First and then Str(End_Idx) = ' ' loop
         End_Idx := End_Idx - 1;
      end loop;
      
      if Start_Idx > End_Idx then
         return "";
      else
         return Str(Start_Idx..End_Idx);
      end if;
   end Trim;
   
   function Parse_Expression (Expression : String) return Result_Type is
      Trimmed_Expr : constant String := Trim(Expression);
      No_Spaces_Expr : constant String := Remove_Spaces(Trimmed_Expr);
      Result : Result_Type;
      
      function Find_Lowest_Precedence_Op (Expr : String) return Integer is
         Depth : Integer := 0;
         Lowest_Precedence : Integer := 999;
         Lowest_Op_Pos : Integer := 0;
      begin
         for I in Expr'Range loop
            case Expr(I) is
               when '(' =>
                  Depth := Depth + 1;
               when ')' =>
                  if Depth > 0 then
                     Depth := Depth - 1;
                  else
                     Result.Error := Invalid_Expression;
                     Result.Error_Message := To_Unbounded_String("Parantezler eşleşmiyor");
                     return -1;
                  end if;
               when '+' | '-' =>
                  if Depth = 0 then
                     if I = Expr'First or else 
                        (Expr(I-1) /= ')' and then not (Expr(I-1) >= '0' and Expr(I-1) <= '9')) then
                        null;
                     elsif Lowest_Precedence >= 1 then
                        Lowest_Precedence := 1;
                        Lowest_Op_Pos := I;
                     end if;
                  end if;
               when '*' | '/' =>
                  if Depth = 0 and then Lowest_Precedence >= 2 then
                     Lowest_Precedence := 2;
                     Lowest_Op_Pos := I;
                  end if;
               when others =>
                  null;
            end case;
         end loop;
         
         if Depth /= 0 then
            Result.Error := Invalid_Expression;
            Result.Error_Message := To_Unbounded_String("Parantezler eşleşmiyor");
            return -1;
         end if;
         
         return Lowest_Op_Pos;
      end Find_Lowest_Precedence_Op;
   begin
      if No_Spaces_Expr'Length = 0 then
         Result.Error := Invalid_Expression;
         Result.Error_Message := To_Unbounded_String("Boş ifade");
         return Result;
      end if;
      
      if not (No_Spaces_Expr'Length >= 2 and then 
         No_Spaces_Expr(No_Spaces_Expr'First) = '(' and then 
         No_Spaces_Expr(No_Spaces_Expr'Last) = ')') and then
         (for all C of No_Spaces_Expr => C /= '+' and C /= '-' and C /= '*' and C /= '/') then
         begin
            Result.Value := Integer'Value(No_Spaces_Expr);
            return Result;
         exception
            when others =>
               Result.Error := Invalid_Expression;
               Result.Error_Message := To_Unbounded_String("Geçersiz sayı: " & No_Spaces_Expr);
               return Result;
         end;
      end if;
      
      if No_Spaces_Expr'Length >= 2 and then 
         No_Spaces_Expr(No_Spaces_Expr'First) = '(' and then 
         No_Spaces_Expr(No_Spaces_Expr'Last) = ')' then
         return Parse_Expression(No_Spaces_Expr(No_Spaces_Expr'First+1..No_Spaces_Expr'Last-1));
      end if;
      
      declare
         Op_Pos : constant Integer := Find_Lowest_Precedence_Op(No_Spaces_Expr);
         Left_Result, Right_Result : Result_Type;
      begin
         if Op_Pos <= 0 then
            if Result.Error /= None then
               return Result;
            end if;
            
            begin
               Result.Value := Integer'Value(No_Spaces_Expr);
               return Result;
            exception
               when others =>
                  Result.Error := Invalid_Expression;
                  Result.Error_Message := To_Unbounded_String("İfade anlaşılamadı: " & No_Spaces_Expr);
                  return Result;
            end;
         end if;
         
         Left_Result := Parse_Expression(No_Spaces_Expr(No_Spaces_Expr'First..Op_Pos-1));
         if Left_Result.Error /= None then
            return Left_Result;
         end if;
         
         Right_Result := Parse_Expression(No_Spaces_Expr(Op_Pos+1..No_Spaces_Expr'Last));
         if Right_Result.Error /= None then
            return Right_Result;
         end if;
         
         case No_Spaces_Expr(Op_Pos) is
            when '+' =>
               Result.Value := Left_Result.Value + Right_Result.Value;
            when '-' =>
               Result.Value := Left_Result.Value - Right_Result.Value;
            when '*' =>
               Result.Value := Left_Result.Value * Right_Result.Value;
            when '/' =>
               if Right_Result.Value = 0 then
                  Result.Error := Division_By_Zero;
                  Result.Error_Message := To_Unbounded_String("Sıfıra bölme hatası");
                  return Result;
               end if;
               Result.Value := Left_Result.Value / Right_Result.Value;
            when others =>
               Result.Error := Invalid_Expression;
               Result.Error_Message := To_Unbounded_String("Bilinmeyen operatör");
               return Result;
         end case;
         
         return Result;
      end;
   end Parse_Expression;
   
   Input : String(1..100);
   Last  : Natural;
   Result : Result_Type;
begin
   Put_Line("Çıkmak için 'exit' yazın");
   
   loop
      Put("-> ");
      Get_Line(Input, Last);
      
      exit when Last = 0;
      
      declare
         Command : constant String := Input(1..Last);
      begin
         if Command = "exit" or Command = "quit" then
            exit;
         else
            Result := Parse_Expression(Command);
            
            if Result.Error /= None then
               Put_Line("Hata: " & To_String(Result.Error_Message));
            else
               Put("= ");
               Put(Result.Value);
               New_Line;
            end if;
         end if;
      end;
   end loop;
end Calculator;