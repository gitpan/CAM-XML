package CAM::XML;

use 5.006;
use strict;
use warnings;
use CAM::XML::Text;
use English qw(-no_match_vars);
use Carp;

our $VERSION = '1.13';

=head1 NAME 

CAM::XML - Encapsulation of a simple XML data structure

=head1 LICENSE

Copyright 2005 Clotho Advanced Media, Inc., <cpan@clotho.com>

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 SYNOPSIS

  my $pollTag = CAM::XML->new('poll');
  
  foreach my $q (@questions) {
    my $questionTag = CAM::XML->new('question');
    
    $questionTag->add(-text => $q->{text});
    my $choicesTag = CAM::XML->new('choices');
    
    foreach my $c (@{$q->{choices}}) {
      my $choiceTag = CAM::XML->new('choice');
      $choiceTag->setAttribute('value', $c->{value});
      $choiceTag->add(-text => $c->{text});
      $choicesTag->add($choiceTag);
    }
    $questionTag->add($choicesTag);
    $pollTag->add($questionTag);
  }
  print CAM::XML->header();
  print $pollTag->toString();

=head1 DESCRIPTION

This module reads and writes XML into a simple object model.  It is
optimized for ease of creating code that interacts with XML.

This module is not as powerful or as standards-compliant as say
XML::LibXML, XML::SAX, XML::DOM, etc, but it's darn easy to use.  I
recommend it to people who want to just read/write a quick but valid
XML file and don't want to bother with the bigger modules.

In our experience, this module is actually easier to use than
XML::Simple because the latter makes some assumptions about XML
structure that prevents it from handling all XML files well.  YMMV.

However, one exception to the simplicity claimed above is our
implementation of a subset of XPath.  That's not very simple.  Sorry.

=head1 CLASS METHODS

=over

=item parse XMLSTRING

=item parse -string => XMLSTRING

=item parse -filename => XMLFILENAME

=item parse -filehandle => XMLFILEHANDLE

Parse an incoming stream of XML into a CAM::XML heirarchy.  This
method just hands the first argument off to XML::Parser, so it can
accept any style of arg that XML::Parser can.  Note that XML::Parser
says the filehandle style should pass an IO::Handle object.  This can
be called as a class method or an instance method.

Additional meaningful flags:

  -cleanwhitespace => 1

Traverse the document and remove non-significant whitespace, as per
removeWhitespace().

  -xmlopts => HASHREF

Any options in this hash are passed directly to XML::Parser.

NOTE: this method does NOT work well on subclasses.  I tried, but
failed to fix it up.  The problems is that CAM::XML::XMLTree has to be
able to instantiate one of this class, but there's no really good way
to communicate with it yet.

=cut

sub parse
{
   my $pkg_or_self = shift;
   my $mode;
   if ($_[0] && $_[0] =~ /^-/)    # If no mode was specified, imply one
   {
      $mode = shift;
   }
   else
   {
      $mode = '-string';
   }
   my $xml   = shift;
   my %flags = (@_);

   local $SIG{__DIE__};
   local $SIG{__WARN__};
   require CAM::XML::XMLTree;
   eval { require XML::Parser; };
   if ($EVAL_ERROR)
   {
      croak 'Failed to load XML::Parser';
   }
   my $pkg = ref $pkg_or_self || $pkg_or_self;
   my $p = XML::Parser->new(Style => $pkg.'::XMLTree',
                            $flags{-xmlopts} ? %{$flags{xmlopts}} : ());
   my $self;
   if ($mode eq '-filename')
   {
      if (open my $fh, '<', $xml)
      {
         local $INPUT_RECORD_SEPARATOR = undef;
         $self = $p->parse(<$fh>);
         close $fh;
      }
   }
   else
   {
      $self = $p->parse($xml);
   }
   if ($self && $flags{-cleanwhitespace})
   {
      $self->removeWhitespace();
   }
   return $self;
}

=item new tagname

=item new tagname, key => value, key => value, ...

Create a new XML tag.  Optionally, you can set tag attributes at the
same time.

=cut

sub new
{
   my $pkg  = shift;
   my $name = shift;

   if (!$name)
   {
      croak 'No XML tag name specified';
   }

   my $self = bless {
      name       => $name,
      attributes => {},
      children   => [],
   }, $pkg;

   return $self->setAttributes(@_);
}

=item header

Return a string containing the following message, suffixed by a newline:

  <?xml version="1.0" encoding="UTF-8" standalone="no" ?>

=cut

sub header
{
   return qq[<?xml version="1.0" encoding="UTF-8" standalone="no" ?>\n];
}

=back

=head1 INSTANCE METHODS

=over

=item getName

Returns the name of the node.

=cut

sub getName
{
   my $self = shift;
   return $self->{name};
}

=item setAttributes key, value, key => value, ...

Set the value of one or more XML attributes.  If any keys are
duplicated, only the last one set is recorded.

=cut

sub setAttributes
{
   my $self = shift;

   while (@_ > 0)
   {
      my $key   = shift;
      my $value = shift;
      if (!$key)
      {
         croak 'Invalid key specified';
      }
      $self->{attributes}->{$key} = $value;
   }
   return $self;
}

=item getAttributeNames

Returns a list of the names of all the attributes of this node.  The
names are returned in arbitrary order.

=cut

sub getAttributeNames
{
   my $self = shift;

   return keys %{ $self->{attributes} };
}

=item getAttributes

Returns a hash of all attributes.

=cut

sub getAttributes
{
   my $self = shift;

   return %{ $self->{attributes} };
}

=item getAttribute KEY

Returns the value of the named attribute, or undef if it does not exist.

=cut

sub getAttribute
{
   my $self = shift;
   my $key  = shift;

   return $key ? $self->{attributes}->{$key} : undef;
}

=item getChildren

Returns an array of XML nodes and text objects contained by this node.

=cut

sub getChildren
{
   my $self = shift;
   return @{ $self->{children} };
}

=item getChild INDEX

Returns a child of this node.  The argument is a zero-based index.
Returns undef if the index is not valid.

=cut

sub getChild
{
   my $self  = shift;
   my $index = shift;

   return if (!defined $index || $index !~ /^\d+$/);
   return $self->{children}->[$index];
}

=item getChildNodes

Returns an array of XML nodes contained by this node (that is, unlike
getChildren(), text nodes are ignored).

=cut

sub getChildNodes
{
   my $self = shift;

   return grep { $_->isa(__PACKAGE__) } @{ $self->{children} };
}

=item getChildNode INDEX

Returns a CAM::XML child of this node (that is, unlike getChild(),
text nodes are ignored.  The argument is a zero-based index.  Returns
undef if the index is not valid.

=cut

sub getChildNode
{
   my $self  = shift;
   my $index = shift;

   return if (!defined $index || $index !~ /^\d+$/);
   my @kids = grep { $_->isa(__PACKAGE__) } @{ $self->{children} };
   return $kids[$index];
}

=item setChildren VALUE, VALUE, ...

Removes all the children from this node and replaces them with the
supplied values.  All of the values MUST be CAM::XML or CAM::XML::Text
objects, or this method will abort and return false before any changes
are made.

=cut

sub setChildren
{
   my $self = shift;

   my @bad = grep { !(defined $_ && ref $_ && 
                   ($_->isa(__PACKAGE__) || $_->isa('CAM::XML::Text'))) } @_;
   return if (@bad > 0);

   @{ $self->{children} } = @_;
   return $self;
}

=item add CAM::XML object

=item add -text => text

=item add -cdata => text

=item add -xml => rawxml

=item add <multiple elements of the above types>

Add content within the current tag.  Order of addition may be
significant.  This content can be any one of 1) subsidiary XML tags
(CAM::XML), 2) literal text content (-text or -cdata), or 3)
pre-formatted XML content (-xml).

In -text and -cdata content, any reserved characters will be
automatically escaped.  Those two modes differ only in their XML
representation: -cdata is more human-readable if there are a lot of
"&", "<" and ">" characters in your text, where -text is usally more
compact for short strings.  These strings are not escaped until
output.

Content in -xml mode is parsed in as CAM::XML objects.  If it is not
valid XML, a warning will be emitted and the add will fail.

=cut

sub add
{
   my $self = shift;

   while (@_ > 0)
   {
      my $add = shift;
      if (!$add)
      {
         croak 'Undefined object';
      }
      elsif (ref $add)
      {
         unless ($add->isa(__PACKAGE__) || $add->isa('CAM::XML::Text'))
         {
            croak 'Invalid object type to add to a CAM::XML tag (' . ref $add . ')';
         }
         push @{ $self->{children} }, $add;
      }
      else
      {
         if ($add =~ /^-(text|cdata)$/)
         {

            # If the previous element was the same kind of text item
            # then merge them.  Otherwise append this text item.

            my $type = $1;
            my $text = shift;
            if (@{ $self->{children} } > 0 &&
                $self->{children}->[-1]->isa('CAM::XML::Text') &&
                $self->{children}->[-1]->{type} eq $type)
            {
               $self->{children}->[-1]->{text} .= $text;
            }
            else
            {
               push @{ $self->{children} },
                   CAM::XML::Text->new($type => $text);
            }
         }
         elsif ($add eq '-xml')
         {
            my $xml    = shift;
            my $parsed = $self->parse($xml);
            if ($parsed)
            {
               $self->add($parsed);
            }
            else
            {
               croak 'Tried to add invalid XML content';
            }
         }
         else
         {
            croak "Unknown flag '$add'.  Expected '-text' or '-cdata' or '-xml'";
         }
      }
   }

   return $self;
}

=item removeWhitespace

Clean out all non-significant whitespace.  Whitespace is deemed
non-significant if it is bracketed by tags.  This might not be true in
some data formats (e.g. HTML) so don't use this function carelessly.

=cut

sub removeWhitespace
{
   my $self = shift;

   my @delete  = ();
   my $lasttag = -1;
   foreach my $i (0 .. $#{ $self->{children} })
   {
      my $child = $self->{children}->[$i];
      if ($child->isa(__PACKAGE__))
      {
         if (defined $lasttag)
         {
            push @delete, ($lasttag + 1) .. ($i - 1);
         }
         $child->removeWhitespace();
         $lasttag = $i;
      }
      elsif (defined $child->{text} && $child->{text} =~ /\S/)
      {
         $lasttag = undef;
      }
   }
   if (defined $lasttag)
   {
      push @delete, ($lasttag + 1) .. $#{ $self->{children} };
   }
   while (@delete > 0)
   {
      splice @{ $self->{children} }, pop(@delete), 1;
   }
   return $self;
}

=item getInnerText

For the given node, descend through all of its children and
concatenate all the text values that are found.  If none, this method
returns an empty string (not undef).

=cut

sub getInnerText
{
   my $self = shift;

   my $text  = q{};
   my @stack = ([@{ $self->{children} }]);
   while (@stack > 0)
   {
      my $list  = $stack[-1];
      my $child = shift @$list;
      if ($child)
      {
         if ($child->isa(__PACKAGE__))
         {
            push @stack, [@{ $child->{children} }];
         }
         elsif (defined $child->{text})
         {
            $text .= $child->{text};
         }
      }
      else
      {
         pop @stack;
      }
   }
   return $text;
}

=item getNodes -tag => TAGNAME

=item getNodes -attr => ATTRNAME, -value => ATTRVALUE

=item getNodes -path => PATH

Return an array of CAM::XML objects representing nodes that match the
requested properties.

A path is a syntactic path into the XML doc something like an XPath:

  '/' divides nodes
  '//' means any number of nodes
  '/[n]' means the nth child of a node (1-based)
  '<tag>[n]' means the nth instance of this node
  '/[-n]' means the nth child of a node, counting backward
  '/[last()]' means the last child of a node (same as [-1])
  '/[@attr="value"]' means a node with this attribute value
  '/text()' means all of the text data inside a node
            (note this returns just one node, not all the nodes)

For example, C</html/body//table/tr[1]/td/a[@target="_blank"]>
searches an XHTML body for all tables, and returns all anchor nodes in
the first row which pop new windows.

Please note that while this syntax resembles XPath, it is FAR from a
complete (or even correct) implementation.  It's useful for basic
delving into an XML document, however.

=cut

sub getNodes
{
   my $self     = shift;
   my %criteria = (@_);

   if ($criteria{-path})
   {
      # This is a very different beast.  Handle it separately.
      return $self->_get_path_nodes($criteria{-path}, [$self]);
   }

   my @list  = ();
   my @stack = ([$self]);
   while (@stack > 0)
   {
      my $list = $stack[-1];
      my $obj  = shift @$list;
      if ($obj)
      {
         if ($obj->isa(__PACKAGE__))
         {
            push @stack, [@{ $obj->{children} }];
            if (($criteria{-tag} && $criteria{-tag} eq $obj->{name}) ||
                ($criteria{-attr} && exists $obj->{attributes}->{$criteria{-attr}} &&
                 $obj->{attributes}->{$criteria{-attr}} eq $criteria{-value}))
            {
               push @list, $obj;
            }
         }
      }
      else
      {
         pop @stack;
      }
   }
   return @list;
}

# Internal use only

sub _get_path_nodes
{
   my $self = shift;
   my $path = shift;
   my $kids = shift || $self->{children};

   my @list;
   if (!$path)
   {
      push @list, $self;
   }

   elsif ($path =~ m,^/?text\(\)$,)
   {
      # Note: this is a COPY of the data, not the data itself.  I
      # *think* that's the right thing to do, but I'm not quite sure.

      push @list, CAM::XML::Text->new(text => $self->getInnerText());
   }

   elsif ($path =~ m,^/?\[(\d+)\](.*)$,)
   {
      # this is a special case of the next elsif
      # it's higher performance since we can go straight to the
      # index instead of looping
      my $num   = $1;
      my $rest  = $2;
      my $match = $kids->[$num - 1];
      if ($match)
      {
         push @list, $match->_get_path_nodes($rest);
      }
   }

   elsif ($path =~ m,^/?\[([^\]]+)\](.*)$,)
   {
      my $limit = $1;
      my $rest  = $2;

      my $index = 0;
      foreach my $node (@$kids)
      {
         ++$index;    # one-based
         if ($self->_match($node, undef, $limit, $index, scalar @$kids))
         {
            push @list, $node->_get_path_nodes($rest);
         }
      }
   }

   elsif ($path =~ m,^//+$,)
   {
      foreach my $node (@$kids)
      {
         if ($node->isa(__PACKAGE__))
         {
            push @list, $node, $node->_get_path_nodes($path);
         }
      }
   }

   elsif ($path =~ m,(/?)(/?)([^/]+)(.*)$,)
   {
      my $base  = $1;
      my $any   = $2;
      my $match = $3;
      my $rest  = $4;

      my $limit = undef;
      if ($match =~ s,\[([^\]]+)\]$,,)
      {
         $limit = $1;
         if (!$limit)
         {
            croak "bad index in path (indices are one-based): '$path'";
         }
      }
      if ($match && $limit)
      {
         # This is a special case that arose from a bug in _match()
         # TODO: move the @group and $index logic into _match()
         my @group;
         my $index = 0;
         my $max   = 0;
         foreach my $node (@$kids)
         {
            ++$index;    # one-based
            if ($self->_match($node, $match, undef, $index, scalar @$kids))
            {
               push @group, 1;
               $max++;
            }
            else
            {
               push @group, 0;
            }
         }
         $index = 0;
         foreach my $i (0 .. $#$kids)
         {
            my $node = $kids->[$i];
            if ($group[$i])
            {
               ++$index;    # one-based
               if ($self->_match($node, undef, $limit, $index, $max))
               {
                  push @list, $node->_get_path_nodes($rest);
               }
            }
            if ($any)
            {
               push @list, $node->_get_path_nodes($path);
            }
         }
      }
      elsif ($match || $limit)
      {
         my $index = 0;
         foreach my $node (@$kids)
         {
            ++$index;    # one-based
            if ($self->_match($node, $match, $limit, $index, scalar @$kids))
            {
               push @list, $node->_get_path_nodes($rest);
            }
            if ($any)
            {
               push @list, $node->_get_path_nodes($path);
            }
         }
      }
      else
      {
         die 'Internal error: neither match nor limit were true';
      }
   }
   else
   {
      croak "path not understood: '$path'";
   }
   return @list;
}
sub _match
{
   my $self  = shift;
   my $node  = shift;
   my $tag   = shift;
   my $limit = shift;
   my $index = shift;    # one-based
   my $max   = shift;

   if ($tag && $limit)
   {
      die 'Internal error: _match() is broken for simultaneous tag and index matches';
      # currently, the $tag && $limit case is handled externally.
      # TODO: handle this better
   }

   my $is_element = $node->isa(__PACKAGE__);
   if ($tag)
   {
      return if (!$is_element);
      return if ($node->{name} ne $tag);
   }
   if ($limit)
   {
      # massaging
      if ($limit eq 'last()')
      {
         $limit = -1;
      }

      if ($limit =~ /^\-\d+/)
      {
         return if ($max + $limit + 1 != $index);
      }
      elsif ($limit =~ /^\d+/)
      {
         return if ($limit != $index);
      }
      elsif ($limit =~ /^\@(\w+)=\"([^\"]*)\"$/ ||
             $limit =~ /^\@(\w+)=\'([^\']*)\'$/)
      {
         return if (!$is_element);
         my $attr = $1;
         my $val  = $2;
         my $cmp  = $node->{attributes}->{$attr};
         return if (!defined $cmp || $val ne $cmp);
      }
      else
      {
         croak "path predicate not understood: '$limit'";
      }
   }
   return $self;
}

=item toString [OPTIONS...]

Serializes the tag and all subsidiary tags into an XML string.  This
is called recursively on any subsidiary CAM::XML objects.  Note that
the XML header is not prepended to this output.

The following optional arguments apply:

  -formatted => boolean
        If true, the XML is indented nicely.  Otherwise, no whitespace
        is inserted between tags.
  -textformat => boolean
        Only relevent if -formatted is true.  If false, this prevents
        the formatting of pure text values.
  -level => number
        Indents this tag by the number of levels indicated.  This implies
        -formatted => 1
  -indent => number
        The number of spaces to indent per level if the output is
        formatted.  By default, this is 2 (i.e. two spaces).

Example: -formatted => 0

   <foo><bar>Baz</bar></foo>

Example: -formatted => 1

   <foo>
     <bar>
       Baz
     </bar>
   </foo>

Example: -formatted => 1, textformat => 0

   <foo>
     <bar>Baz</bar>
   </foo>
Example: -formatted => 1, textformat => 0, -indent => 4

   <foo>
       <bar>Baz</bar>
   </foo>

=cut

sub toString
{
   my $self = shift;
   my %args = (@_);

   if ($args{'-formatted'} && !exists $args{'-level'})
   {
      $args{'-level'} = 0;
      if (!exists $args{'-textformat'})
      {
         $args{'-textformat'} = 1;
      }
   }
   if (!$args{'-indent'} || $args{'-indent'} =~ /\D/)
   {
      $args{'-indent'} = 2;
   }

   return join q{}, $self->_to_string(%args);
}

sub _to_string
{
   my $self = shift;
   my %args = (@_);

   my $level  = $args{'-level'};
   my $indent = defined $level ? q{ } x $args{'-indent'} : q{};
   my $begin  = defined $level ? $indent x $level        : q{};
   my $end    = defined $level ? "\n"                    : q{};

   # open tag
   my @ret = ( $begin, '<', $self->_XML_escape($self->{name}) );

   # attributes
   foreach my $key (sort keys %{ $self->{attributes} })
   {
      push @ret, (
         q{ }, $self->_XML_escape($key), q{=},
         q{"}, $self->_XML_escape($self->{attributes}->{$key}), q{"},
      );
   }

   # Empty tag?
   if (@{ $self->{children} } == 0)
   {
      push @ret, '/>', $end;
   }

   # Body is pure text?
   elsif ($args{'-formatted'} && !$args{'-textformat'}
          && 0 == scalar grep {$_->isa(__PACKAGE__)} @{$self->{children}})
   {
      push @ret, '>';
      push @ret, map { $_->toString() } @{ $self->{children} };
      push @ret, '</', $self->{name}, '>', $end;
   }

   # Body has elements
   else
   {
      push @ret, '>', $end;
      foreach my $child (@{ $self->{children} })
      {
         if ($child->isa(__PACKAGE__))
         {
            push @ret, $child->_to_string(
               %args, -level => defined $level ? $level+1 : undef,
            );
         }
         else
         {
            push @ret, $begin, $indent, $child->toString(), $end;
         }
      }
      push @ret, $begin, '</', $self->{name}, '>', $end;
   }

   return @ret;
}

# Private function
sub _XML_escape
{
   my $pkg_or_self = shift;
   my $text        = shift;

   if (!defined $text)
   {
      $text = q{};
   }
   $text =~ s/&/&amp;/g;
   $text =~ s/</&lt;/g;
   $text =~ s/>/&gt;/g;
   $text =~ s/\"/&quot;/g;
   return $text;
}

# Private function
sub _CDATA_escape
{
   my $pkg_or_self = shift;
   my $text        = shift;

   # Escape illegal "]]>" strings by ending and restarting the CDATA section
   $text =~ s/\]\]>/]]>]]&gt;<![CDATA[/g;

   return "<![CDATA[$text]]>";
}

1;
__END__

=back

=head1 ENCODING

It is assumed that all text will be UTF-8.  This includes any tag
names, attribute keys and values, text content, and raw XML content
that are added to the data structure.

=head1 CODING

This module has just over 90% code coverage in its regression tests,
as reported by L<Devel::Cover> via C<perl Build testcover>.  The
remaining 10% is mostly error conditions and a few conditional
defaults.

This module passes many of the Perl Best Practices guidelines, as
enforced by L<Perl::Critic> v0.09.  Notable exceptions are the
legacy camelCase subroutine names.

=head1 AUTHOR

Clotho Advanced Media Inc., I<cpan@clotho.com>

Primary Developer: Chris Dolan
