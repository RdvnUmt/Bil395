use strict;
use warnings;

my $ERR_NONE = 0;
my $ERR_INVALID_EXPR = 1;
my $ERR_DIV_BY_ZERO = 2;

sub parse_expression {
    my ($expr) = @_;
    $expr =~ s/\s+//g; 
    
    if ($expr eq '') {
        return (0, $ERR_INVALID_EXPR, "Boş ifade");
    }
    
    if ($expr =~ /^[0-9]+$/) {
        return (int($expr), $ERR_NONE, "");
    }
    
    if ($expr =~ /^\((.+)\)$/) {
        return parse_expression($1);
    }
    
    my $depth = 0;
    my $lowest_precedence = 999;
    my $lowest_op_pos = -1;
    
    my @chars = split //, $expr;
    for (my $i = 0; $i < scalar @chars; $i++) {
        my $c = $chars[$i];
        if ($c eq '(') {
            $depth++;
        } elsif ($c eq ')') {
            if ($depth > 0) {
                $depth--;
            } else {
                return (0, $ERR_INVALID_EXPR, "Parantezler eşleşmiyor");
            }
        } elsif (($c eq '+' || $c eq '-') && $depth == 0) {
            if ($i == 0 || $chars[$i-1] !~ /[0-9\)]/) {
                next;
            }
            
            if ($lowest_precedence >= 1) {
                $lowest_precedence = 1;
                $lowest_op_pos = $i;
            }
        } elsif (($c eq '*' || $c eq '/') && $depth == 0) {
            if ($lowest_precedence >= 2) {
                $lowest_precedence = 2;
                $lowest_op_pos = $i;
            }
        }
    }
    
    if ($depth != 0) {
        return (0, $ERR_INVALID_EXPR, "Parantezler eşleşmiyor");
    }
    
    if ($lowest_op_pos >= 0) {
        my $left_expr = substr($expr, 0, $lowest_op_pos);
        my $right_expr = substr($expr, $lowest_op_pos + 1);
        my $op = $chars[$lowest_op_pos];
        
        my ($left, $left_err, $left_err_msg) = parse_expression($left_expr);
        if ($left_err != $ERR_NONE) {
            return ($left, $left_err, $left_err_msg);
        }
        
        my ($right, $right_err, $right_err_msg) = parse_expression($right_expr);
        if ($right_err != $ERR_NONE) {
            return ($right, $right_err, $right_err_msg);
        }
        
        if ($op eq '+') {
            return (int($left + $right), $ERR_NONE, "");
        } elsif ($op eq '-') {
            return (int($left - $right), $ERR_NONE, "");
        } elsif ($op eq '*') {
            return (int($left * $right), $ERR_NONE, "");
        } elsif ($op eq '/') {
            if ($right == 0) {
                return (0, $ERR_DIV_BY_ZERO, "Sıfıra bölme hatası");
            }
            return (int($left / $right), $ERR_NONE, "");
        }
    }
    
    return (0, $ERR_INVALID_EXPR, "İfade anlaşılamadı: $expr");
}

print "Çıkmak için 'exit' yazın\n";

while (1) {
    print "> ";
    my $input = <STDIN>;
    chomp($input);
    
    last if $input =~ /^\s*$/;
    
    if ($input eq "exit" || $input eq "quit") {
        last;
    } else {
        my ($result, $error, $error_msg) = parse_expression($input);
        
        if ($error != $ERR_NONE) {
            print "Hata: $error_msg\n";
        } else {
            print "= $result\n";
        }
    }
} 