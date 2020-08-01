program alg;

type
    algebraic = record
        case is_left: boolean of
            true: (l: integer);
            false: (r: char);
    end;
    lfunc_type = function(v: integer): boolean;
    rfunc_type = function(v: char): boolean;
var x, y: algebraic;

function in_L(v: integer): algebraic;
begin
    result.is_left := true;
    result.l := v;
end;

function in_R(v: char): algebraic;
begin
    result.is_left := false;
    result.r := v;
end;

function alg_case(sum: algebraic; lfunc: lfunc_type; rfunc: rfunc_type): boolean;
begin
    case sum.is_left of
        true: alg_case := lfunc(sum.l);
        false: alg_case := rfunc(sum.r);
    end;
end;

function is_odd(x: integer): boolean;
begin
    result := x mod 2 <> 0;
end;

function is_letter_a(x: char): boolean;
begin
    result := (x = 'a') or (x = 'A');
end;

begin
    x := in_L(15);
    y := in_R('A');
    writeln(alg_case(x, is_odd, is_letter_a));
    writeln(alg_case(y, is_odd, is_letter_a));
end.
