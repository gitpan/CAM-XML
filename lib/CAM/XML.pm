package CAM::XML;

=head1 NAME 

CAM::XML - Encapsulation of a simple XML data structure

=head1 LICENSE

Copyright 2005 Clotho Advanced Media, Inc., <cpan@clotho.com>

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 SYNOPSIS

  my $pollTag = CAM::XML->new("poll");
  
  foreach my $q (@questions) {
    my $questionTag = CAM::XML->new("question");
    
    $questionTag->add(-text => $q->{text});
    my $choicesTag = CAM::XML->new("choices");
    
    foreach my $c (@{$q->{choices}}) {
      my $choiceTag = CAM::XML->new("choice");
      $choiceTag->setAttribute("value", $c->{value});
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

=cut

require 5.005_62;
use strict;
use warnings;
use Carp;

our @ISA = qw();
our $VERSION = '1.12';

#-----------------

=head1 CLASS METHODS

=over 4

=cut

#-----------------

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
   if ($_[0] && $_[0] =~ /^-/)  # If no mode was specified, imply one
   {
      $mode = shift;
   }
   else
   {
      $mode = "-string";
   }
   my $xml = shift;
   my %flags = (@_);
   
   local $SIG{__DIE__};
   local $SIG{__WARN__};

   eval "use XML::Parser";
   if ($@)
   {
      &carp("Failed to load XML::Parser");
      return undef;
   }
   my $pkg = ref($pkg_or_self) || $pkg_or_self;
   my $p = XML::Parser->new(Style => $pkg."::XMLTree",
                            $flags{-xmlopts} ? %{$flags{xmlopts}} : ());
   my $self;
   if ($mode eq "-filename")
   {
      local *FILE;
      if (open(FILE, "<$xml"))
      {
         local $/ = undef;
         local $SIG{__DIE__};
         local $SIG{__WARN__};
         $self = $p->parse(<FILE>);
         close(FILE);
      }
   }
   else
   {
      local $SIG{__DIE__};
      local $SIG{__WARN__};
      $self = $p->parse($xml);
   }
   if ($self && $flags{-cleanwhitespace})
   {
      $self->removeWhitespace();
   }
   return $self;
}
#-----------------

=item new tagname

=item new tagname, key => value, key => value, ...

Create a new XML tag.  Optionally, you can set tag attributes at the
same time.

=cut

sub new
{
   my $pkg = shift;
   my $name = shift;

   if (!$name) {
      &carp("No XML tag name specified\n");
      return undef;
   }

   my $self = bless({
      name => $name,
      attributes => {},
      children => [],
   }, $pkg);

   return $self->setAttributes(@_);
}
#-----------------

=item header

Return a string containing the following message, suffixed by a newline:

  <?xml version="1.0" encoding="UTF-8" standalone="no" ?>

=cut

sub header
{
   return qq[<?xml version="1.0" encoding="UTF-8" standalone="no" ?>\n];
}
#-----------------

=back

=head1 INSTANCE METHODS

=over 4

=item getName

Returns the name of the node.

=cut

sub getName
{
   my $self = shift;
   return $self->{name};
}
#-----------------

=item setAttributes key, value, key => value, ...

Set the value of one or more XML attributes.  If any keys are
duplicated, only the last one set is recorded.

=cut

sub setAttributes
{
   my $self = shift;

   while (@_ > 0)
   {
      my $key = shift;
      my $value = shift;
      if (!$key)
      {
         &carp("Invalid key spcified");
         return undef;
      }
      $self->{attributes}->{$key} = $value;
   }
   return $self;
}
#-----------------

=item getAttributeNames

Returns a list of the names of all the attributes of this node.  The
names are returned in arbitrary order.

=cut

sub getAttributeNames
{
   my $self = shift;

   return keys %{$self->{attributes}};
}
#-----------------

=item getAttributes

Returns a hash of all attributes.

=cut

sub getAttributes
{
   my $self = shift;

   return (%{$self->{attributes}});
}
#-----------------

=item getAttribute KEY

Returns the value of the named attribute, or undef if it does not exist.

=cut

sub getAttribute
{
   my $self = shift;
   my $key = shift;

   return $key ? $self->{attributes}->{$key} : undef;
}
#-----------------

=item getChildren

Returns an array of XML nodes and text objects contained by this node.

=cut

sub getChildren
{
   my $self = shift;
   return @{$self->{children}};
}
#-----------------

=item getChild INDEX

Returns a child of this node.  The argument is a zero-based index.
Returns undef if the index is not valid.

=cut

sub getChild
{
   my $self = shift;
   my $index = shift;

   return undef unless (defined $index && $index =~ /^\d+$/);
   return $self->{children}->[$index];
}
#-----------------

=item getChildNodes

Returns an array of XML nodes contained by this node (that is, unlike
getChildren(), text nodes are ignored).

=cut

sub getChildNodes
{
   my $self = shift;

   return grep {$_->isa("CAM::XML")} @{$self->{children}};
}
#-----------------

=item getChildNode INDEX

Returns a CAM::XML child of this node (that is, unlike getChild(),
text nodes are ignored.  The argument is a zero-based index.  Returns
undef if the index is not valid.

=cut

sub getChildNode
{
   my $self = shift;
   my $index = shift;

   return undef unless (defined $index && $index =~ /^\d+$/);
   return( (grep {$_->isa("CAM::XML")} @{$self->{children}})[$index] );
}
#-----------------

=item setChildren VALUE, VALUE, ...

Removes all the children from this node and replaces them with the
supplied values.  All of the values MUST be CAM::XML or CAM::XML::Text
objects, or this method will abort and return false before any changes
are made.

=cut

sub setChildren
{
   my $self = shift;

   my @bad = grep({!(defined($_) && ref($_) && 
                     ($_->isa("CAM::XML") || $_->isa("CAM::XML::Text")))} @_);
   return undef if (@bad > 0);

   @{$self->{children}} = @_;
   return $self;
}
#-----------------

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
         &carp("Undefined object");
         return undef;
      }
      elsif (ref($add))
      {
         unless ($add->isa(__PACKAGE__) || $add->isa("CAM::XML::Text"))
         {
            &carp("Invalid object type to add to a CAM::XML tag (" . 
                  ref($add) . ")");
            return undef;
         }
         push @{$self->{children}}, $add;
      }
      else
      {
         if ($add =~ /^-(text|cdata)$/)
         {
            # If the previous element was the same kind of text item
            # then merge them.  Otherwise append this text item.

            my $type = $1;
            my $text = shift;
            if (@{$self->{children}} > 0 &&
                $self->{children}->[-1]->isa("CAM::XML::Text") &&
                $self->{children}->[-1]->{type} eq $type)
            {
               $self->{children}->[-1]->{text} .= $text;
            }
            else
            {
               push @{$self->{children}}, CAM::XML::Text->new($type => $text);
            }
         }
         elsif ($add eq "-xml")
         {
            my $xml = shift;
            my $parsed = $self->parse($xml);
            if ($parsed)
            {
               $self->add($parsed);
            }
            else
            {
               &carp("Tried to add invalid XML content");
            }
         }
         else
         {
            &carp("Unknown flag '$add'.  Expected '-text' or '-cdata' or '-xml'");
            return undef;
         }
      }
   }

   return $self;
}
#-----------------

=item removeWhitespace

Clean out all non-significant whitespace.  Whitespace is deemed
non-significant if it is bracketed by tags.  This might not be true in
some data formats (e.g. HTML) so don't use this function carelessly.

=cut

sub removeWhitespace
{
   my $self = shift;

   my @delete = ();
   my $lasttag = -1;
   foreach my $i (0 .. $#{$self->{children}})
   {
      my $child = $self->{children}->[$i];
      if ($child->isa("CAM::XML"))
      {
         if (defined $lasttag)
         {
            push @delete, ($lasttag+1) .. ($i-1);
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
      push @delete, ($lasttag+1) .. $#{$self->{children}};
   }
   while (@delete > 0)
   {
      splice @{$self->{children}}, pop(@delete), 1;
   }
   return $self;
}
#-----------------

=item getInnerText

For the given node, descend through all of its children and
concatenate all the text values that are found.  If none, this method
returns an empty string (not undef).

=cut

sub getInnerText
{
   my $self = shift;

   my $text = "";
   my @stack = ([@{$self->{children}}]);
   while (@stack > 0) {
      my $list = $stack[-1];
      my $child = shift @$list;
      if ($child)
      {
         if ($child->isa("CAM::XML"))
         {
            push @stack, [@{$child->{children}}];
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
#-----------------

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
   my $self = shift;
   my %criteria = (@_);

   if ($criteria{-path})
   {
      # This is a very different beast.  Handle it separately.
      return $self->getPathNodes($criteria{-path}, [$self]);
   }

   my @list = ();
   my @stack = ([$self]);
   while (@stack > 0)
   {
      my $list = $stack[-1];
      my $obj = shift @$list;
      if ($obj)
      {
         if ($obj->isa("CAM::XML"))
         {
            push @stack, [@{$obj->{children}}];
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
#-----------------

# Internal use only

sub getPathNodes
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
      my $num = $1;
      my $rest = $2;
      my $match = $kids->[$num-1];
      if ($match)
      {
         push @list, $match->getPathNodes($rest);
      }
   }
   elsif ($path =~ m,^/?\[([^\]]+)\](.*)$,)
   {
      my $limit = $1;
      my $rest = $2;

      my $index = 0;
      foreach my $node (@$kids)
      {
         ++$index; # one-based
         if ($self->match($node, undef, $limit, $index, scalar @$kids))
         {
            push @list, $node->getPathNodes($rest);
         }
      }
   }
   elsif ($path =~ m,(/?)(/?)([^/]+)(.*)$,)
   {
      my $base = $1;
      my $any = $2;
      my $match = $3;
      my $rest = $4;
 
      my $limit = undef;
      if ($match =~ s,\[([^\]]+)\]$,,)
      {
         $limit = $1;
      }
      if ($match && $limit)
      {
         # This is a special case that arose from a bug in match()
         # TODO: move the @group and $index logic into match()
         my @group;
         my $index = 0;
         my $max = 0;
         foreach my $node (@$kids)
         {
            ++$index; # one-based
            if ($self->match($node, $match, undef, $index, scalar @$kids))
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
         foreach my $i (0..$#$kids)
         {
            my $node = $kids->[$i];
            if ($group[$i])
            {
               ++$index; # one-based
               if ($self->match($node, undef, $limit, $index, $max))
               {
                  push @list, $node->getPathNodes($rest);
               }
            }
            if ($any)
            {
               push @list, $node->getPathNodes($path);
            }
         }
      }
      elsif ($match || $limit)
      {
         my $index = 0;
         foreach my $node (@$kids)
         {
            ++$index; # one-based
            if ($self->match($node, $match, $limit, $index, scalar @$kids))
            {
               push @list, $node->getPathNodes($rest);
            }
            if ($any)
            {
               push @list, $node->getPathNodes($path);
            }
         }
      }
      elsif ($any)
      {
         foreach my $node (@$kids)
         {
            push @list, $node->getPathNodes($path);
         }
      }
   }
   else
   {
      &carp("path not understood: '$path'");
   }
   return @list;
}
sub match
{
   my $self = shift;
   my $node = shift;
   my $tag = shift;
   my $limit = shift;
   my $index = shift;  # one-based
   my $max = shift;

   if ($tag && $limit)
   {
      die "Error: match() is broken for simultaneous tag and index matches";
      # currently, the $tag && $limit case is handled externally.
      # TODO: handle this better
   }

   my $isElement = $node->isa(__PACKAGE__);
   if ($tag)
   {
      return undef if (!$isElement);
      return undef if ($node->{name} ne $tag);
   }
   if ($limit)
   {
      # massaging
      if ($limit eq "last()")
      {
         $limit = -1;
      }

      if ($limit =~ /^\-\d+/)
      {
         return undef if ($max+$limit+1 != $index);
      }
      elsif ($limit =~ /^\d+/)
      {
         return undef if ($limit != $index);
      }
      elsif ($limit =~ /^\@(\w+)=\"([^\"]*)\"$/ ||
             $limit =~ /^\@(\w+)=\'([^\']*)\'$/)
      {
         return undef if (!$isElement);
         my $attr = $1;
         my $val = $2;
         my $cmp = $node->{attributes}->{$attr};
         return undef if (!defined $cmp || $val ne $cmp);
      }
      else
      {
         &carp("path predicate not understood: '$limit'");
      }
   }
   return $self;
}
#-----------------

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

   if ($args{"-formatted"} && (!exists $args{"-level"}))
   {
      $args{"-level"} = 0;
      $args{"-textformat"} = 1 if (!exists $args{"-textformat"});
   }
   my $level = $args{"-level"};
   $args{"-indent"} = 2 if ((!$args{"-indent"}) || $args{"-indent"} =~ /\D/);
   my $indent = defined $level ? " " x $args{"-indent"} : "";
   my $begin = defined $level ? $indent x $level : "";
   my $end = defined $level ? "\n" : "";
   
   my $string = $begin . "<" . $self->XMLescape($self->{name});
   foreach my $key (sort keys %{$self->{attributes}})
   {
      $string .= " " . $self->XMLescape($key) . "=\"" . $self->XMLescape($self->{attributes}->{$key}) . "\"";
   }
   if (@{$self->{children}} == 0)
   {
      $string .= "/>" . $end;
   }
   elsif ($args{"-formatted"} && !$args{"-textformat"} && scalar(grep {$_->isa("CAM::XML")} @{$self->{children}}) == 0)
   {
      $string .= ">" . join("", map({$_->toString()} @{$self->{children}})) . "</" . $self->{name} . ">" . $end;
   }
   else
   {
      $string .= ">" . $end;
      foreach my $child (@{$self->{children}})
      {
         if ($child->isa("CAM::XML"))
         {
            $string .= $child->toString(%args, -indent => $args{"-indent"},
                                        (defined $level ? 
                                        (-level => $level+1) : ()));
         }
         else
         {
            $string .= $begin . $indent . $child->toString() . $end;
         }
      }
      $string .= $begin . "</" . $self->{name} . ">" . $end;
   }
   return $string;
}
#-----------------

# Private function
sub XMLescape
{
   my $pkg_or_self = shift;
   my $text = shift;

   $text = "" if (!defined $text);
   $text =~ s/&/&amp;/g;
   $text =~ s/</&lt;/g;
   $text =~ s/>/&gt;/g;
   $text =~ s/\"/&quot;/g;
   return $text;
}
#-----------------

# Private function
sub CDATAescape
{
   my $pkg_or_self = shift;
   my $text = shift;

   # Escape illegal "]]>" strings by ending and restarting the CDATA section
   $text =~ s/\]\]>/]]>]]&gt;<![CDATA[/g;

   return "<![CDATA[$text]]>";
}
#-----------------


# ----------------------------------------------------------------------

# This is a helper class to hold text data

package CAM::XML::Text;

sub new
{
   my $pkg = shift;
   my $type = shift;
   my $text = shift;

   $text = "" if (!defined $text);
   return bless({type => $type, text => $text}, $pkg);
}
#-----------------

sub toString
{
   my $self = shift;
   return ($self->{type} eq "text" ? 
           CAM::XML->XMLescape($self->{text}) : 
           ($self->{type} eq "cdata" ? 
            CAM::XML->CDATAescape($self->{text}) :
            $self->{text}));
}
#-----------------

sub getInnerText()
{
   my $self = shift;
   return $self->{text};
}
#-----------------

sub match
{
   die "Shouldn't get here";
}
#-----------------

sub getPathNodes
{
   my $self = shift;
   my $path = shift;

   return $path ? () : ($self);
}
#-----------------



# ----------------------------------------------------------------------

# This is a modified copy of XML::Parser::Tree from XML/Parser.pm
# v2.30.  It differs from the original in that each object is one
# hashref instead of one scalar and one scalar/arrayref in the
# objectlist.

package CAM::XML::XMLTree;
use Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(Init Start End Char CdataStart CdataEnd Final);

sub Init
{
   my $expat = shift;
   $expat->{Stack} = [];
   $expat->{Package} = "CAM::XML";
}

sub Start
{
   my $expat = shift;
   my $tag = shift;
   my $obj = $expat->{Package}->new($tag, @_);
   $expat->{Base} ||= $obj;
   if (@{$expat->{Stack}} > 0)
   {
      $expat->{Stack}->[-1]->add($obj);
   }
   push @{$expat->{Stack}}, $obj;
}

sub End
{
   my $expat = shift;
   my $tag = shift;
   pop @{$expat->{Stack}};
   delete $expat->{Cdata};
}

sub Char
{
   my $expat = shift;
   my $text = shift;
   
   $expat->{Stack}->[-1]->add(($expat->{Cdata} ? "-cdata" : "-text") => $text);
}

sub CdataStart
{
   my $expat = shift;
   $expat->{Cdata} = 1;
}

sub CdataEnd
{
   my $expat = shift;
   delete $expat->{Cdata};
}

sub Final
{
   my $expat = shift;
   delete $expat->{Stack};
   return delete $expat->{Base};
}
#-----------------

1;
__END__

=back

=head1 ENCODING

It is assumed that all text will be UTF-8.  This includes any tag
names, attribute keys and values, text content, and raw XML content
that are added to the data structure.

=head1 AUTHOR

Clotho Advanced Media Inc., I<cpan@clotho.com>

Primary Developer: Chris Dolan
