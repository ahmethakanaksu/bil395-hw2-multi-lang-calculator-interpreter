#!/usr/bin/perl
use strict;
use warnings;

my %vars;

print "Perl Calculator Interpreter\n";
print "Supports +, -, *, /, ^, (), variables (x = 5 + 2)\n";
print "Type 'exit' to quit.\n\n";

while (1) {
    print ">> ";
    my $input = <STDIN>;
    chomp($input);
    last if $input eq 'exit';
    next if $input =~ /^\s*$/;

    if ($input =~ /^\s*([a-zA-Z][a-zA-Z0-9]*)\s*=\s*(.+)$/) {
        my ($var, $expr) = ($1, $2);
        my $result = evaluate($expr);
        if (defined $result) {
            $vars{$var} = $result;
            print "$var = $result\n";
        }
    } elsif ($input =~ /=/) {
        print "Invalid assignment syntax or variable name. Use format like: x = 5 + 3\n";
        next;
    } else {
        my $result = evaluate($input);
        print "Result: $result\n" if defined $result;
    }
}

sub evaluate {
    my ($expr) = @_;
    my @tokens = tokenize($expr);
    unless (@tokens) {
        print "Syntax Error: Could not tokenize expression.\n";
        return;
    }
    my $postfix = infix_to_postfix(@tokens);
    return unless defined $postfix;
    return eval_postfix(@$postfix);
}

sub tokenize {
    my ($expr) = @_;
    my @tokens;
    while ($expr =~ /\G\s*([A-Za-z][A-Za-z0-9]*|\d+(?:\.\d+)?|[\+\-\*\/\^\(\)]|\S)/gc) {
        push @tokens, $1;
    }
    return @tokens;
}

sub precedence {
    my ($op) = @_;
    return 3 if $op eq '^';
    return 2 if $op eq '*' || $op eq '/';
    return 1 if $op eq '+' || $op eq '-';
    return 0;
}

sub infix_to_postfix {
    my @tokens = @_;
    my @output;
    my @stack;

    foreach my $token (@tokens) {
        if ($token =~ /^\d/ || $token =~ /^[a-zA-Z][a-zA-Z0-9]*$/) {
            push @output, $token;
        } elsif ($token eq '(') {
            push @stack, $token;
        } elsif ($token eq ')') {
            while (@stack && $stack[-1] ne '(') {
                push @output, pop @stack;
            }
            if (!@stack || $stack[-1] ne '(') {
                print "Syntax Error: Mismatched parentheses.\n";
                return;
            }
            pop @stack;
        } elsif ($token =~ /^[\+\-\*\/\^]$/) {
            while (@stack &&
                   (precedence($stack[-1]) > precedence($token) ||
                   (precedence($stack[-1]) == precedence($token) && $token ne '^'))) {
                push @output, pop @stack;
            }
            push @stack, $token;
        } else {
            print "Syntax Error: Invalid token '$token'\n";
            return;
        }
    }

    while (@stack) {
        if ($stack[-1] eq '(' || $stack[-1] eq ')') {
            print "Syntax Error: Mismatched parentheses.\n";
            return;
        }
        push @output, pop @stack;
    }

    return \@output;
}

sub eval_postfix {
    my @postfix = @_;
    my @stack;

    foreach my $token (@postfix) {
        if ($token =~ /^\d/ || $token =~ /^[a-zA-Z][a-zA-Z0-9]*$/) {
            my $val = ($token =~ /^\d/) ? $token : $vars{$token};
            if (!defined $val) {
                print "Unknown variable: $token\n";
                return;
            }
            push @stack, $val;
        } elsif ($token =~ /^[\+\-\*\/\^]$/) {
            my $b = pop @stack;
            my $a = pop @stack;
            unless (defined $a && defined $b) {
                print "Syntax Error: Insufficient values.\n";
                return;
            }
            if ($token eq '/' && $b == 0) {
                print "Error: Division by zero!\n";
                return;
            }
            my $res = eval "$a $token $b";
            $res = $a ** $b if $token eq '^';
            push @stack, $res;
        }
    }

    return @stack == 1 ? $stack[0] : do {
        print "Syntax Error: Invalid expression.\n";
        undef;
    };
}
