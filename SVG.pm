package GD::SVG;

use strict;
use Carp 'croak','carp';
use GD;
use SVG;
use warnings;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $AUTOLOAD);
require Exporter;

#$VERSION = sprintf "0.%02d", q$Revision: 1.7 $ =~ /(\d+)/g;
$VERSION = sprintf "0.%d%d", q$Revision: 1.7 $ =~ /(\d+)/g;
@ISA = qw(Exporter);

%EXPORT_TAGS = ('cmp'  => [qw(GD_CMP_IMAGE 
			      GD_CMP_NUM_COLORS
			      GD_CMP_COLOR
			      GD_CMP_SIZE_X
			      GD_CMP_SIZE_Y
			      GD_CMP_TRANSPARENT
			      GD_CMP_BACKGROUND
			      GD_CMP_INTERLACE
			      GD_CMP_TRUECOLOR
			     )
			  ]
	       );

@EXPORT = qw(
	     gdStyled
	     gdBrushed
	     gdTransparent
	     gdTinyFont
	     gdSmallFont
	     gdMediumBoldFont
	     gdLargeFont
	     gdGiantFont
	     gdDashSize
	     gdMaxColors
	     gdStyledBrushed
	     gdTiled
	    );

# Not yet implemented
#@EXPORT_OK = qw (
#		 GD_CMP_IMAGE 
#		 GD_CMP_NUM_COLORS
#		 GD_CMP_COLOR
#		 GD_CMP_SIZE_X
#		 GD_CMP_SIZE_Y
#		 GD_CMP_TRANSPARENT
#		 GD_CMP_BACKGROUND
#		 GD_CMP_INTERLACE
#		 GD_CMP_TRUECOLOR
#		);


# GD does not allow dynamic creation of fonts. These default values
# are approximate sizes for the various fonts based on an extensive
# afternoon of cross-comparison ;)

use constant DEFAULT_FONT       => 'Helvetica';
use constant TINY_HEIGHT        => 8;
use constant TINY_WIDTH         => 5;
use constant TINY_WEIGHT        => 'normal';
use constant SMALL_HEIGHT       => 11; # originally 12
use constant SMALL_WIDTH        => 6;
use constant SMALL_WEIGHT       => 'normal';
use constant MEDIUM_BOLD_HEIGHT => 13;
use constant MEDIUM_BOLD_WIDTH  => 7;
use constant MEDIUM_BOLD_WEIGHT => 'bold';
use constant LARGE_HEIGHT       => 16;
use constant LARGE_WIDTH        => 8;
use constant LARGE_WEIGHT       => 'normal';
use constant GIANT_HEIGHT       => 15;
use constant GIANT_WEIGHT       => 'bold';
use constant GIANT_WIDTH        => 8;

# TEXT_KLUDGE controls the number of pixels to bump text on the
# Y-axis in order to more closely match GD output.
use constant TEXT_KLUDGE       => '2';

#########################
# END CONSTANTS - No user serviceable options below this point
#########################

# Trap GD methods that are not yet implemented in SVG.pm
sub AUTOLOAD {
  my $self = shift;
  warn "GD method $AUTOLOAD is not implemented in GD::SVG";
}

# There must be a better way to trap these errors
sub _error {
  my $method = shift;
  warn "GD method $method is not implemented in GD::SVG";
}

#############################
# GD::SVG::Image methods
#############################
sub GD::SVG::Image::new {
  my ($self,$width,$height) = @_;
  my $this = bless {},$self;
  my $img = SVG->new(width=>$width,height=>$height);
  $this->{img} = $img;
  $this->{width} = $width;
  $this->{height} = $height;

  # Let's create an internal representation of the image in GD
  # so that I can easily use some of GD's methods
  $this->{internal_gd} = GD::Image->new($width,$height);

  # Let's just assume that we always want the foreground color to be
  # black This, for the most part, works for Bio::Graphics This
  # certainly needs to be fixed...
  $this->{foreground} = $this->colorAllocate('0','0','0');
  return $this;
}


#############################
# Image Data Output Methods #
#############################
sub GD::SVG::Image::svg {
  my $self = shift;
  my $img = $self->{img};
  $img->xmlify(-pubid => "-//W3C//DTD SVG 1.0//EN",
               -inline   => 1);
}

#############################
# Color management routines #
#############################
sub GD::SVG::Image::colorAllocate {
  my ($self,$r,$g,$b) = @_;
  $r ||= 0;
  $g ||= 0;
  $b ||= 0;
  my $color = "rgb($r,$g,$b)";
  my $gd = _internal_gd($self);
  my $gd_color = $gd->colorAllocate($r,$g,$b);

  # Save GDs color_index, keyed by the RGB triplet that SVG utilizes.
  $self->{color_index}->{$color} = $gd_color;
  return $color;
}

sub GD::SVG::Image::colorAllocateAlpha { _error('colorAllocateAlpha'); }

sub GD::SVG::Image::colorDeallocate {
  my ($self,$index) = @_;
  my $colors = %{$self->{color_index}};
  my $gd = _internal_gd($self);
  $gd->colorDeallocate($index);
  delete $colors->{$index};
}

# workaround for bad GD
#sub colorClosest {
# NOT YET IMPLEMENTED
sub GD::SVG::Image::colorClosest {
  my ($self,$gd,@c) = @_;
  # Let's just return the color for now.
  # Terrible kludge.
  my $color = $self->colorAllocate(@c);
  return $color;
  #  my ($self,$gd,@c) = @_;
  #  return $self->{closestcache}{"@c"} if exists $self->{closestcache}{"@c"};
  #  return $self->{closestcache}{"@c"} = $gd->colorClosest(@c) if $GD::VERSION < 2.04;
  #  my ($value,$index);
  #  for (keys %COLORS) {
  #    my ($r,$g,$b) = @{$COLORS{$_}};
  #    my $dist = ($r-$c[0])**2 + ($g-$c[1])**2 + ($b-$c[2])**2;
  #    ($value,$index) = ($dist,$_) if !defined($value) || $dist < $value;
  #  }
  #  return $self->{closestcache}{"@c"} = $self->{translations}{$index};
}

sub GD::SVG::Image::colorClosestHWB { _error('colorClosestHWB'); }

sub GD::SVG::Image::colorExact {
  my ($self,$r,$g,$b) = @_;
  my $color = $self->colorAllocate($r,$g,$b);
  if ($color) {
    return $color;
  } else {
    return ('-1');
  }
}

sub GD::SVG::Image::colorResolve    { _error('colorResolve'); }

sub GD::SVG::Image::colorsTotal {
  my $self = shift;
  my $gd = _internal_gd($self);
  return $gd->colorsTotal;
}


# NOT YET IMPLEMENTED
sub GD::SVG::Image::getPixel {
  my ($self,$x,$y) = @_;
  # I don't have any cogent way to fetch the value of an asigned pixel
  # without calculating all positions and loading into memory.

  # For these purposes, I could maybe just look it up...  From a hash
  # table or something - Keep track of all assigned pixels and their
  # color...
  return ('rgb(255,255,255)');
}

# Given the color index, return its rgb index...  GD::SVG does not
# pass indices, instead using stringified RGB triplets.
sub GD::SVG::Image::rgb {
  my ($self,$index) = @_;
  # Invert the internal color index hash
  my %indices = map { $self->{color_index}->{$_} => $_ } keys %{$self->{color_index}};
  my $color = $indices{$index};
  return $color;
}

sub GD::SVG::Image::transparent { _error('transparent'); }

#######################
# Special Colors
#######################
sub GD::SVG::Image::setBrush { _error('setBrush'); }

# Lines in GD are 1 pixel in diameter by default.
# setThickness allows line thickness to be changed.
# This should be retained until it's changed again
# Each method should check the thickness of the line...
sub GD::SVG::Image::setThickness {
  my ($self,$thickness) = @_;
  $self->{line_thickness} = $thickness;
}

# There is no direct translation of gdStyled.  In gd, this is used to
# set the style for the line usiing the settings of the current brush.
# Drawing with the new style is then used by passing the gdStyled as a
# color.
# I could probably implement this in SVG using polyline.
sub GD::SVG::Image::setStyle {
  my ($self,@params) = @_;
  $self->{current_style} = [ @params ];
  return;
}



##################################################
# Exported methods that belong in Main namespace #
##################################################

# In GD, the gdStyled method allows one to draw with a styled line
# Here we will simply return the format of the line along with a flag
# so that appropriate subroutines can deal with it.

# Similarly, the gdTransparent method lets users introduce gaps in
# lines. I'll handle it similarly to gdStyled...
# This might just be as simple as setting the color to the background color.
# (This, of course, will not work for syled lines).
sub gdStyled { return 'styled'; }
sub gdBrushed { return 'brushed'; }

sub gdTiled       { _error('gdTiled'); }
sub gdDashSize    { _error('gdDashSize'); }
sub gdMaxColors   { _error('gdMaxColors'); }
sub gdStyledBrush { _error('gdStyledBrush'); }

sub gdTransparent { return 'transparent'; }


sub gdAntiAliased { _error('gdAntiAliased'); }
sub GD::SVG::Image::setAntiAliased { _error('setAntiAliased'); }
sub GD::SVG::Image::setAntiAliasedDontBlend { _error('setAntiAliasedDontBlend'); }

#######################
# Drawing subroutines #
#######################
# setPixel is actually just drawing a circle
sub GD::SVG::Image::setPixel {
  my ($self,$x1,$y1,$color,$color2) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  $img->circle(cx=>$x1,cy=>$y1,r=>'0.03',
	       id=>$id,
	       style=>{
		       'stroke'=>$color,
		       'fill'=>$color,
		       # 'stroke-opacity'=>'0.5',
		       'fill-opacity'=>'1.0'
		      }
	      );
}


sub GD::SVG::Image::line {
  my ($self,$x1,$y1,$x2,$y2,$color) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  
  my $thickness = _get_thickness($self) || 1;
  # Are we trying to draw with a styled line? If so,
  # lets fetch the style and then use the polyline method.
  # Drawing with styled lines is not yet implemented
  if ($color eq 'styled' || $color eq 'transparent') {
    #  $self->_fetch_style();   # This will be used for generating styled lines
    $img->line(x1=>$x1,y1=>$y1,
	       x2=>$x2,y2=>$y2,
	       id=>$id,
	       style => {
			 'stroke'         => $color,
			 'stroke-opacity' => '1',
			 'stroke-width'   => $thickness,
			 'fill'           => $color,
			 'fill-opacity'   => '1',
			}
	      );
  } else {
    $img->line(x1=>$x1,y1=>$y1,
	       x2=>$x2,y2=>$y2,
	       id=>$id,
	       style => {
			 'stroke' => $color,
			 'stroke-opacity' => '1',
			 'stroke-width'   => $thickness,
			 'fill'   => $color,
			 'fill-opacity'   => '1',
			}
	      );
  }
}


sub GD::SVG::Image::dashedLine { _error('dashedLine'); }

sub GD::SVG::Image::rectangle {
  my ($self,$x1,$y1,$x2,$y2,$color) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  my $foreground = $self->{foreground};
  my $fg = ($color eq $foreground) ? $foreground : $color;
  my $thickness = _get_thickness($self) || 1;
  $img->rectangle(x=>$x1,y=>$y1,
		  width  =>$x2-$x1,
		  height =>$y2-$y1,
		  id     =>$id,
		  style  => {
			     'stroke'         => $color,
			     'stroke-width'   => $thickness,
			     'stroke-opacity' => '1',
			     'fill-opacity'   => '0'
			    }
		 );
}

# This is EXACTLY the same as the rectangle method above
# I should just use it but pass a flag for the fill opacity
sub GD::SVG::Image::filledRectangle {
  my ($self,$x1,$y1,$x2,$y2,$color) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  my $thickness = _get_thickness($self) || 1;
  $img->rectangle(x=>$x1,y=>$y1,
		  width  =>$x2-$x1,
		  height =>$y2-$y1,
		  id     =>$id,
		  style  => {
			     #stroke =>$self->{foreground},
			     stroke =>$color,
			     'stroke-width' => $thickness,
			     fill   =>$color,
			     # 'stroke-opacity' => '0',
			     # 'fill-opacity'   => '0'
			    }
		 );
}

sub GD::SVG::Image::polygon {
  my ($self,$poly,$stroke) = @_;
  my $img = $self->{img};
  
  # Create seperate x and y arrays of vertices
  my @xpoints = $poly->fetch_vertices('x');
  my @ypoints = $poly->fetch_vertices('y');
  my $points = $img->get_path(
			    x=>\@xpoints, y=>\@ypoints,
			    -type=>'polygon'
			   );
  my $id = $self->_create_id($xpoints[0],$ypoints[0]);
  my $thickness = _get_thickness($self) || 1;
  $img->polygon(
		%$points,
		id=>$id,
		style=>{
			'stroke'=>$stroke,
			'stroke-opacity'=>'1.0',
			'stroke-width' => $thickness,			
			'fill'=>$stroke,
			'fill-opacity'=>'0'
		       }
	       )
}

# Passing the stroke doesn't really work as expected...
sub GD::SVG::Image::filledPolygon {
  my ($self,$poly,$fill,$stroke) = @_;
  my $img = $self->{img};

  # Create seperate x and y arrays of vertices
  my @xpoints = $poly->fetch_vertices('x');
  my @ypoints = $poly->fetch_vertices('y');
  my $points = $img->get_path(
			    x=>\@xpoints, y=>\@ypoints,
			    -type=>'polygon'
			   );
  my $id = $self->_create_id($xpoints[0],$ypoints[0]);
  my $thickness = _get_thickness($self) || 1;
  $img->polygon(
		%$points,
		id=>$id,
		style=>{
			'stroke'=>$stroke,
			'stroke-opacity'=>'1.0',
			'stroke-width' => $thickness,			
			'fill'=>$fill,
			'fill-opacity'=>'1.0'
		       }
	       )
}

sub GD::SVG::Image::ellipse {
  my ($self,$x1,$y1,$width,$height,$color) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  # GD uses width and height - SVG uses radii...
  $width  = $width / 2;
  $height = $height / 2;
  my $thickness = _get_thickness($self) || 1;
  $img->ellipse(
		cx=>$x1, cy=>$y1,
		rx=>$width, ry=>$height,
		id=>$id,
		style=>{
			'stroke'=>$color,
			'stroke-width' => $thickness,
			'stroke-opacity'=>'1.0',
			'fill-opacity'=>'0'
		       }
	       );
}

sub GD::SVG::Image::filledEllipse {
  my ($self,$x1,$y1,$width,$height,$color) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  # GD uses width and height - SVG uses radii...
  $width  = $width / 2;
  $height = $height / 2;
  my $thickness = _get_thickness($self) || 1;
  $img->ellipse(
		cx=>$x1, cy=>$y1,
		rx=>$width, ry=>$height,
		id=>$id,
		style=>{
			'stroke'=>$color,
			'stroke-width' => $thickness,
			'stroke-opacity'=>'1.0',
			'fill'=>$color,
			'fill-opacity'=>'1.0'
		       }
	       );
}

# I cannot easily create arcs via SVG.pm...
# Can i pass parameters anonymously?

# the arc method calls SVGs elipse method
# Can this method create ellipses as well?
sub GD::SVG::Image::arc {
  my ($self,$x1,$y1,$width,$height,$start,$end,$color) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  # GD uses width and height - SVG uses radii...
  $width  = $width  / 2;
  $height = $height / 2;

  # This needs to create two ellipses based on the start and end
  # and then transform them to bring out the arc...
  my $thickness = _get_thickness($self) || 1;
  $img->path('x'=>$x1,'y'=>$y1,'rx'=>$width,'ry'=>$height,
	     style=>{
		     'stroke'=>$color,
		     'stroke-width'=> $thickness,
		     'fill'=>$color,
		    }
	    );
}

sub GD::SVG::Image::filledArc    { _error('filledArc'); }
sub GD::SVG::Image::fill         { _error('fill'); }
sub GD::SVG::Image::fillToBorder { _error('fillToBorder'); }


##################################################
# Image Copying Methods
##################################################

# None implemented

##################################################
# Image Transformation Methods
##################################################

# None implemented

##################################################
# Character And String Drawing
##################################################
sub GD::SVG::Image::string {
  my ($self,$font_obj,$x,$y,$text,$color,$transform) = @_;
  my $img = $self->{img};
  my $id = $self->_create_id($x,$y);
  my $formatting = $font_obj->formatting();
  $transform ||= 'rotate(0)';
  $img->text(
	     id=>$id,
	     x=>$x,
	     y=>$y + $font_obj->{height} - TEXT_KLUDGE,
	     %$formatting,
	     transform => $transform,
	    )->cdata($text);
}

# I don't think this is positioned correctly...
sub GD::SVG::Image::stringUp {
  my ($self,@rest) = @_;
  $self->string(@rest,'rotate(-90)');
}

sub GD::SVG::Image::char {
  my ($self,@rest) = @_;
  $self->string(@rest);
}

sub GD::SVG::Image::charUp {
  my ($self,@rest) = @_;
  $self->string(@rest,'rotate(-90)');
}

# Replicating the TrueType handling
#sub GD::Image::stringFT { _error('stringFT'); }


##################################################
# Alpha Channels
##################################################
sub GD::SVG::Image::alphaBlending { _error('alphaBlending'); }
sub GD::SVG::Image::saveAlpha     { _error('saveAlpha'); }

##################################################
# Miscellaneous Image Methods
##################################################
sub GD::SVG::Image::interlaced { _error('inerlaced'); }

sub GD::SVG::Image::getBounds {
  my $self = shift;
  my $width = $self->{width};
  my $height = $self->{height};
  return($width,$height);
}

sub GD::SVG::Image::isTrueColor { _error('isTrueColor'); }
sub GD::SVG::Image::compare     { _error('compare'); }
sub GD::SVG::Image::clip        { _error('clip'); }
sub GD::SVG::Image::boundsSafe  { _error('boundsSafe'); }


################################
# Font Factories and Utilities
################################
sub GD::SVG::gdTinyFont {
  my $this = bless {},'GD::SVG::Font';
  $this->{font}   = DEFAULT_FONT;
  $this->{height} = TINY_HEIGHT;
  $this->{width}  = TINY_WIDTH;
  $this->{weight} = TINY_WEIGHT;
  return $this;
}

sub GD::SVG::gdSmallFont {
  my $this = bless {},'GD::SVG::Font';
  $this->{font} = DEFAULT_FONT;
  $this->{height} = SMALL_HEIGHT;
  $this->{width}  = SMALL_WIDTH;
  $this->{weight}  = SMALL_WEIGHT;
  return $this;
}

sub GD::SVG::gdMediumBoldFont {
  my $this = bless {},'GD::SVG::Font';
  $this->{font}   = DEFAULT_FONT;
  $this->{height} = MEDIUM_BOLD_HEIGHT;
  $this->{width}  = MEDIUM_BOLD_WIDTH;
  $this->{weight} = MEDIUM_BOLD_WEIGHT;
  return $this;
}

sub GD::SVG::gdLargeFont {
  my $this = bless {},'GD::SVG::Font';
  $this->{font}   = DEFAULT_FONT;
  $this->{height} = LARGE_HEIGHT;
  $this->{width}  = LARGE_WIDTH;
  $this->{weight} = LARGE_WEIGHT;
  return $this;
}

sub GD::SVG::gdGiantFont {
  my $this = bless {},'GD::SVG::Font';
  $this->{font}   = DEFAULT_FONT;
  $this->{height} = GIANT_HEIGHT;
  $this->{width}  = GIANT_WIDTH;
  $this->{weight} = GIANT_WEIGHT;
  return $this;
}

# The can() method is not supported in GD::SVG
sub GD::SVG::Image::can { return 0; }


##########################################
# Internal routines for meshing with SVG #
##########################################
sub GD::SVG::Image::_create_id {
  my ($self,$x,$y) = @_;
  $self->{id_count}++;
  return (join('-',$self->{id_count},$x,$y));
}


# SVG needs some self-awareness so that post-drawing operations can
# occur. This is accomplished by tracking all of the pixels that have
# been filled in thus far.
sub _save {
  my ($self) = @_;
  
  #  my $path = $img->get_path(x=>[$x1,$x2],y=>[$y1,$y2],-type=>'polyline',-closed=>1);
  #  foreach (keys %$path) {
  #    print STDERR $_,"\t",$path->{$_},"\n";
  #  }
  
  #  push (@{$self->{pixels_filled}},$path);
}

# Get the thickness of the line (if it has been set)
sub _get_thickness {  return shift->{line_thickness} }

# return the internal GD object
sub _internal_gd { return shift->{internal_gd} }


1;



##################################################
# GD::SVG::Polygon
##################################################
package GD::SVG::Polygon;

use strict;
use Carp 'croak','carp';
#use warnings;
use vars qw(@ISA);

@ISA = qw/GD::SVG/;

sub new {
  my $class = shift;
  my $this = bless{},$class;
  # I could create a new GD::Image object for each polygon
  # This would allow me to store an internal representation
  # of the GD object if I wanted to...
  # (as well as utilize GDs methods).
  return $this;
}

# Save all polygon vertices sequentially
sub addPt {
  my ($self,$x,$y) = @_;
  # Let's store these in a slighly more GD-like way
  # Is the index of vertices 0-based?
  my $total = scalar keys %{$self->{vertices}};
  $total ||= 0;
  $self->{vertices}->{$total} = [$x,$y];
  return $total;
}

sub getPt {
  my ($self,$index) = @_;
  my ($x,$y) = @{$self->{vertices}->{$index}};
  return ($x,$y);
}

sub setPt {
  my ($self,$index,$x,$y) = @_;
  $self->{vertices}->{$index} = [$x,$y];
  return $index;
}

sub deletePt {
  my ($self,$index) = @_;
  my ($x,$y) = @{$self->{vertices}->{$index}};
  delete $self->{vertices}->{$index};
  return ($x,$y);
}

sub toPt   { _error('GD::SVG::Polygon::toPt'); }

sub length {
  my $self = shift;
  my @vertices = $self->vertices;
  return scalar @vertices;
}

sub vertices {
  my $self = shift;
  my @vertices;
  my @indices = sort { $a <=> $b } keys %{$self->{vertices}};
  foreach my $index (@indices) {
    push (@vertices,$self->{vertices}->{$index});
  }
  return @vertices;
}

sub fetch_vertices {
  my ($poly,$flag) = @_;
  my @vertices = $poly->vertices;
  my $axis = ($flag eq 'x') ? 0 : 1;
  my @points;
  foreach my $vertex (@vertices) {
    push (@points,$vertex->[$axis]);
  }
  return @points;
}

sub bounds    { _error('GD::SVG::Polygon::bounds'); }

sub offset {
  my ($self,$dx,$dy) = @_;
  # Fetch then remove all the old vertices
  my @vertices = $self->vertices;
  delete $self->{vertices};
  my $c = 0;
  foreach (@vertices) {
    my ($x,$y) = @$_;
    $self->{vertices}->{$c} = [$x+$dx,$y+$dy];
    $c++;
  }
  return $c;
}

sub map       { _error('GD::SVG::Polygon::map'); }
sub transform { _error('GD::SVG::Polygon::transform'); }

sub DESTROY { }

1;


# Generic Font package for accessing height and width information
# and for formatting strings
package GD::SVG::Font;

use vars qw/@ISA/;
@ISA = qw(GD::SVG);

# I NEED TO replicate the package methods for GD::Font->Large (eg)

# Return guestimated values on the font height and width
sub width   { return shift->{width}; }
sub height  { return shift->{height}; }
sub font    { return shift->{font}; }
sub weight  { return shift->{weight}; }
sub nchars  { } # NOT SUPPORTED!!

# Build the formatting hash for each font...
sub formatting {
  my $self = shift;
  my $size    = $self->height;
  my $font    = $self->font;
  my $weight  = $self->weight;
  my %format = ('font-size' => $size,
		'font'       => $font,
	       );
  $format{'font-weight'} = $weight if ($weight);
  return \%format;
}

sub Tiny {
  my $this = GD::SVG::gdTinyFont;
  return $this;
}

sub Small {
  my $this = GD::SVG::gdSmallFont;
  return $this;
}

sub MediumBold {
  return GD::SVG::gdMediumBoldFont;
#  my $this = GD::SVG::gdMediumBoldFont;
#  return $this;
}

sub Large {
  my $this = GD::SVG::gdLargeFont;
  return $this;
}

sub Giant {
  my $this = GD::SVG::gdGiantFont;
  return $this;
}

sub DESTROY { }


1;

=pod

=head1 GD::SVG

GD::SVG - Seamlessly enable SVG output from scripts written using GD

=head1 SYNOPSIS

    # use GD;
    use GD::SVG;

    # my $img = GD::Image->new();
    my $img = GD::SVG::Image->new();

    # $img->png();
    $img->svg();

=head1 DESCRIPTION

GD::SVG painlessly enables scripts that utilize GD to export scalable
vector graphics (SVG). It accomplishes this task by wrapping SVG.pm
with GD-styled method calls. To enable this functionality, one need
only change the "use GD" call to "use GD::SVG" (and initial "new"
method calls).

=head1 EXPORTS

GD::SVG exports the same methods as GD itself overriding those
methods.

=head1 USAGE

In order to generate SVG output from your script using GD::SVG, you
will need to first

     # use GD;
     use GD::SVG;

After that, each call to the package classes that GD implements should
be changed to GD::SVG. Thus:

    GD::Image    becomes  GD::SVG::Image
    GD::Font     becomes  GD::SVG::Font
    GD::Polygon  becomes  GD::SVG::Polygon

=head1 DYAMICALLY SELECTING SVG OUTPUT

If you would like your script to be able to dynamically select either
png or jpeg output (via GD) or SVG output (via GD::SVG), you should
place your "use" statement within an eval. In the example below, each
of the available classes is created at the top of the script for
convenience, as well as the image output type.

    my $package = shift;
    eval "use $package";
    my $image_class = $package . '::Image';
    my $poly_class  = $package . '::Polygon';
    my $font_class  = $package . '::Font';
    
    # Creating new images or polygons thus becomes
    my $image   = $image_class->new($width,$height);
    my $polygon = $poly_class->new();

    # Establish the image output type
    my $image_type;
    if ($package = 'GD::SVG') {
      $image_type = 'svg';
    } else {
      $image_type = 'png';
    }

=head1 IMPORTANT NOTES

GD::SVG does not directly generate SVG, but instead relies upon
SVG.pm. It is not intended to supplant SVG.pm.  Furthermore, since
GD::SVG is, in essence an API to an API, it may not be suitable for
applications where speed is of the essence. In these cases, GD::SVG
may provide a short-term solution while scripts are re-written to
enable more direct output of SVG.

Many of the GD::SVG methods accept additional parameters (which are in
turn reflected in the SVG.pm API) that are not supported in GD.  Look
through the remainder of this document for options on specific In
addition, several functions have yet to be mapped to SVG.pm
calls. Please see the section below regarding regarding GD functions
that are missing or altered in GD::SVG.

=head1 GENERAL DIFFICULTIES IN TRANSLATING GD TO SVG

=over 4

=item SVG requires unique identifiers for each element

Each element in an SVG image requires a unique identifier. In general,
GD::SVG handles this by automatically generating unique random
numbers.  In addition to the typical parameters for GD methods,
GD::SVG methods allow a user to pass an optional id parameter for
naming the object.

Several functions also enable a user to pass a unique name
and number. See the documentation for specific methods below.

=item Direct calls to the GD package will fail

If you use syntax like GD::Image->new in your scripts, these calls
will fail unless you change the module name to GD::SVG::Image->new().

=item "Pan" methods may behave differently

GD "pan" methods like "rectangle" may behave slightly differently in
GD::SVG due to underlying differences in how such objects are created.

=item No support for generation of images from filehandles or raw data

GD::SVG works only with scripts that generate images directly in the
code using the GD->new(height,width) approach.

=item Tiled fills are not supported

Any functions passed gdTiled objects will die.

=item The "arrow" glyph is not rendered properly

Width and color settings for the arrow glyph are ignored. This results
in arrows appearing as 1 pixel black lines in SVG.

=item Styled lines are not implemented

Calls to the gdStyled, gdTransparent, and gdBrushed functions are
supported, although they will have no effect on the resulting
image. Consequently styled lines (such as dashed lines) in PNG images
will appear as solid lines in SVG.

=back

=head1 WHEN THINGS GO WRONG

GD is a complicated module.  Translating gd methods into those
required to draw in SVG are not always direct. You may or may not get
the output you expect. In general, some tweaking of image parameters
(like text height and width) may be necessary.

If your script doesn't work as expected, first check the list of
methods that GD::SVG provides.  Due to differences in the nature of
SVG images, not all GD methods have been implemented in GD::SVG.

If your image doesn't look as expected, try tweaking specific aspects
of image generation.  In particular, check for instances where you
calculate dimensions of items on the fly like font->height. In SVG,
the values of fonts are defined explicitly.

=head1 GD METHODS VERSUS GD::SVG METHODS

All GD::SVG methods mimic the naming and interface of GD methods.  As
such, maintenance of GD::SVG follows the development of both GD and
SVG. Much of the original GD documentation is replicated here for ease
of use. Subtle differnces in the implementation of these methods
between GD and GD::SVG are discussed below. In particular, the return
value for some GD::SVG methods differs from its GD counterpart.

=head2 GD Functions Missing From GD::SVG

The following GD functions have not yet been incorporated into
GD::SVG.

  Creating image objects:

  GD::Image->newPalette([$width,$height])
  GD::Image->newTrueColor([$width,$height])
  GD::Image->newFromPng($file, [$truecolor])
  GD::Image->newFromPngData($data, [$truecolor])
  GD::Image->newFromJpeg($file, [$truecolor])
  GD::Image->newFromJpegData($data, [$truecolor])
  GD::Image->newFromXbm($file)
  GD::Image->newFromWMP($file)
  GD::Image->newFromGd($file)
  GD::Image->newFromGdData($data)
  GD::Image->newFromGd2($file)
  GD::Image->newFromGd2Data($data)
  GD::Image->newFromGd2Part($file,srcX,srcY,width,height)
  GD::Image->newFromXpm($filename)

  Image methods:

  $gddata   = $image->gd
  $gd2data  = $image->gd2
  $wbmpdata = $image->wbmp([$foreground])

=head1 Object Constructors: Creating Images

GD::SVG currently only supports the creation of image objects via its
new constructor.  This is in contrast to GD proper which supports the
creation of images from previous images, filehandles, filenames, and
data.

=over 4

=item $image = GD::SVG::Image->new($height,$width);

Create a blank GD::SVG image object of the specified dimensions in
pixels. In turn, this method will create a new SVG object and store it
internally.

=back

=head1 GD::SVG::Image Methods

Once a GD::Image object is created, you can draw with it, copy it, and
merge two images.  When you are finished manipulating the object, you
can convert it into a standard image file format to output or save to
a file.

=head2 Image Data Output Methods

GD::SVG implements a single output method, svg()!

=over 4

=item  $svg = $image->svg();

This returns the image in SVG format. You may then print it, pipe it
to an image viewer, or write it to a file handle. For example,

      $svg_data = $image->svg;
      open (DISPLAY,"| display -") || die;
      binmode DISPLAY;
      print DISPLAY $svg_data;
      close DISPLAY;

if you'd like to return an inline version of the image (instead of a
full document version complete with the DTD), pass the new method the
'inline' flag:

      $svg_data = $image->svg(-inline=>'true');

Calling the other standard GD image output methods (eg
jpeg,gd,gd2,png) on a GD::SVG::Image object will cause your script to
exit with a warning.

=back

=head2 Color Control

These methods allow you to control and manipulate the color table of a
GD::SVG image. In contrast to GD which uses color indices, GD::SVG
passes stringified RGB triplets as colors. GD::SVG, however, maintains
an internal hash structure of colors and colored indices in order to
map GD functions that manipulate the color table. This typically
requires behind-the-scenes translation of these stringified RGB
triplets into a color index.

=over 4

=item $stringified_color = $image->colorAllocate(RED,GREEN,BLUE)

Unlike GD, colors need not be allocated in advance in SVG.  Unlike GD
which returns a color index, colorAllocate returns a formatted string
compatible with SVG. Simultaneously, it creates and stores internally
a GD compatible color index for use with GD's color manipulation
methods.

     returns: "rgb(RED,GREEN,BLUE)"

=item $index = $image->colorAllocateAlpha

=item $image->colorDeallocate($index)

Provided with a color index, remove it from the color table.

=item $index = $image->colorClosest(red,green,blue)

This returns the index of the color closest in the color table to the
red green and blue components specified. This method is inherited
directly from GD.

   Example: $apricot = $myImage->colorClosest(255,200,180);

=item $index = $image->colorClosestHWB(red,green,blue)

=item $index = $image->colorExact(red,green,blue)

Retrieve the color index of an rgb triplet (or -1 if it has yet to be
allocated).

=item $index = $image->colorResolve(red,green,blue)

=item $colors_total = $image->colorsTotal()

Retrieve the total number of colors indexed in the image.

=item $index = $image->getPixel(x,y)

=item ($red,$green,$blue) = $image->rgb($index)

Provided with a color index, return the RGB triplet.  In GD::SVG,
color indexes are replaced with actual RGB triplets in the form
"rgb($r,$g,$b)".

=item $image->transparent($colorIndex);

Control the transparency of individual colors.

NOT IMPLEMENTED

=back

=head2 Special Colors

GD implements a number of special colors that can be used to achieve
special effects.  They are constants defined in the GD:: namespace,
but automatically exported into your namespace when the GD module is
loaded.

=over 4

=item $image->setBrush($image)

NOT YET IMPLEMENTED

=item $image->setThickness($thickness)

Lines drawn with line(), rectangle(), arc(), and so forth are 1 pixel
thick by default.  Call setThickness() to change the line drawing
width.

=item $image->setStyle(@colors)

NOT YET IMPLEMENTED

=item gdTiled

NOT YET IMPLEMENTED

=item gdStyled

NOT YET IMPLEMENTED

=item gdAntiAliased

NOT YET IMPLEMENTED

=item $image->setAntiAliased($color)

NOT YET IMPLEMENTED

=item $image->setAntiAliasedDontBlend($color,[$flag])

NOT YET IMPLEMENTED

=back

=head2 Drawing Commands

=over 4

=item $image->setPixel($x,$y,$color)

Set the corresponding pixel to the given color.  GD::SVG implements
this by drawing a single dot in the specified color at that position.

=item $image->line(x1,y1,x2,y2,color,$id);

Draw a line between the two coordinate points with the specified
color.  Passing an optional id will set the id of that SVG element. GD
also permits drawing facny lines that are Brushed or Styled.  These
line elements are not supported in GD::SVG.  Instead, the line will
simply become black (and a warning will be returned).

=item $image->dashedLine($x1,$y1,$x2,$y2,$color,$id);

=item $image->rectangle($x1,$y1,$x2,$y2,$color,$id);

This draws a rectangle with the specified color.  (x1,y1) and (x2,y2)
are the upper left and lower right corners respectively.  Only real
colors are supported.

=item $image->filledRectangle($x1,$y1,$x2,$y2,$color,$id);

filledRectangle is a GD specific method with no direct equivalent in
SVG.  GD::SVG translates this method into an SVG appropriate method by
passing the filled color parameter as a named 'filled' parameter to
SVG.
  
   GD call:
     $img->filledRectangle($x1,$y1,$x2,$y2,$color);
  
   SVG call:
     $img->rectangle(x=> $x1,y=> $y1,
		     width  => $x2-$x1,
		     height => $y2-$y1,
		     fill   => $color

=item $image->polygon($polygon,$color);

This draws a polygon with the specified color.  The polygon must be
created first (see "Polygons" below).  The polygon must have at least
three vertices.  If the last vertex doesn't close the polygon, the
method will close it for you.  Both real color indexes and the special
colors gdBrushed, gdStyled and gdStyledBrushed can be specified.

   Example:

     $poly = new GD::::SVG::Polygon;
     $poly->addPt(50,0);
     $poly->addPt(99,99);
     $poly->addPt(0,99);
     $image->polygon($poly,$blue);

=item $image->filledPolygon($color);

This draws a polygon filled with the specified color.  Only real
colors are supported under GD::SVG.

     Example:

     # make a polygon
     $poly = new GD::Polygon;
     $poly->addPt(50,0);
     $poly->addPt(99,99);
     $poly->addPt(0,99);

     # draw the polygon, filling it with a color
     $image->filledPolygon($poly,$peachpuff);

=item $image->ellipse($cx,$cy,$width,$height,$color)
=item $image->filledEllipse($cx,$cy,$width,$height,$color)

These methods() draw ellipses. ($cx,$cy) is the center of the arc, and
($width,$height) specify the ellipse width and height, respec- tively.
filledEllipse() is like ellipse() except that the former produces
filled versions of the ellipse.

=item $image->arc($cy,$cy,$width,$height,$start,$end,$color,$id);

This draws arcs and ellipses.  (cx,cy) are the center of the arc, and
(width,height) specify the width and height, respectively.  The
portion of the ellipse covered by the arc are controlled by start and
end, both of which are given in degrees from 0 to 360.  Zero is at the
top of the ellipse, and angles increase clockwise.  To specify a
complete ellipse, use 0 and 360 as the starting and ending angles.  To
draw a circle, use the same value for width and height.

Internally, arc() calls the ellipse() method of SVG.pm. Although GD
supports drawing with gdBrushed, gdStyled, or gdStyledBrushed, these
options are not supported by GD::SVG.

=item $image->filledArc();

NOT YET FILLED, BUT SHOULD BE ABLE TO SUPPORT

=item $image->fill();

NOT YET IMPLEMENTED BUT SHOULD BE ABLE TO SUPPORT

=item $image->fillToBorder()

NOT YET IMPLEMENTED BUT SHOULD BE ABLE TO SUPPORT

=back

=head2 Image Copying Commands

None of the image copying commands are implemented in GD::SVG.  If
your script calls one of the following methods, your script will die
remorsefully with a warning.  With sufficient demand, I might try to
implement some of these methods.  For now, I think that they are
beyond the intent of GD::SVG.

   $image->copy()
   $image->clone()
   $image->copyMerge()
   $image->copyMergeGray()
   $image->copyResized()
   $image->copyResampled()
   $image->trueColorToPalette()

=head2 Image Transfomation Commands

None of the image transformation commands are implemented in GD::SVG.
If your script calls one of the following methods, your script will
die remorsefully with a warning.  With sufficient demand, I might try
to implement some of these methods.  For now, I think that they are
beyond the intent of GD::SVG.

   $image = $sourceImage->copyRotate90()
   $image = $sourceImage->copyRotate180()
   $image = $sourceImage->copyRotate270()
   $image = $sourceImage->copyFlipHorizontal()
   $image = $sourceImage->copyFlipVertical()
   $image = $sourceImage->copyTranspose()
   $image = $sourceImage->copyReverseTranspose()
   $image->rotate180()
   $image->flipHorizontal()
   $image->flipVertical()

=head2 Character And String Drawing

GD allows you to draw characters and strings, either in normal
horizon- tal orientation or rotated 90 degrees.  In GD, these routines
use a GD::Font object.  Internally, GD::SVG mimics the behavior of GD
with respect to fonts in a very similar manner, using instead a
GD::SVG::Font object described in more detail below.

GD's font handling abilities are not as flexible as SVG and it does
not allow the dynamic creation of fonts, instead exporting five
available fonts as global variables: gdGiantFont, gdLargeFont,
gdMediumBoldFont, gdSmallFont and gdTinyFont. GD::SVG also exports
these same global variables but establishes them in a different manner
using constant variables to establish the font family, font height and
width of these global fonts.  These values were chosen to match as
closely as possible GD's output.  If unsatisfactory, adjust the
constants at the top of this file.  In all subroutines below, GD::SVG
passes a generic GD::SVG::Font object in place of the exported font
variables.

=over 4

=item $image->string($font,$x,$y,$string,$color)

This method draws a string starting at position (x,y) in the speci-
fied font and color.  Your choices of fonts are gdSmallFont,
gdMediumBoldFont, gdTinyFont, gdLargeFont and gdGiantFont.

  Example:

 $myImage->string(gdSmallFont,2,10,"Peachy Keen",$peach);

=item $image->stringUp($font,$x,$y,$string,$color)

Same as the previous example, except that it draws the text rotated
counter-clockwise 90 degrees.

=item $image->char($font,$x,$y,$char,$color)
=item $image->charUp($font,$x,$y,$char,$color)

These methods draw single characters at position (x,y) in the spec-
ified font and color.  They're carry-overs from the C interface, where
there is a distinction between characters and strings.  Perl is
insensible to such subtle distinctions. Neither is SVG, which simply
calls the string() method internally.

=item @bounds = $image->stringFT($fgcolor,$font-
       name,$ptsize,$angle,$x,$y,$string)
=item @bounds = $image->stringFT($fgcolor,$font-
       name,$ptsize,$angle,$x,$y,$string,\%options)

In GD, these methods use TrueType to draw a scaled, antialiased
strings using the TrueType font of your choice. GD::SVG can handle
this directly generating by calling the string() method internally. 

  The arguments are as follows:

  fgcolor    Color index to draw the string in
  fontname   An absolute path to the TrueType (.ttf) font file
  ptsize     The desired point size (may be fractional)
  angle      The rotation angle, in radians
  x,y        X and Y coordinates to start drawing the string
  string     The string itself

GD::SVG attempts to extract the name of the font from the pathname
supplied in the fontname argument. If it fails, Helvetica will be used
instead.

If successful, the method returns an eight-element list giving the
boundaries of the rendered string:

    @bounds[0,1]  Lower left corner (x,y)
    @bounds[2,3]  Lower right corner (x,y)
    @bounds[4,5]  Upper right corner (x,y)
    @bounds[6,7]  Upper left corner (x,y)

This from the GD documentation (not yet implemented in GD::SVG):

An optional 8th argument allows you to pass a hashref of options to
stringFT().  Two hashkeys are recognized: linespacing, if present,
controls the spacing between lines of text.  charmap, if present, sets
the character map to use.

The value of linespacing is supposed to be a multiple of the char-
acter height, so setting linespacing to 2.0 will result in double-
spaced lines of text.  However the current version of libgd (2.0.12)
does not do this.  Instead the linespacing seems to be double what is
provided in this argument.  So use a spacing of 0.5 to get separation
of exactly one line of text.  In practice, a spacing of 0.6 seems to
give nice results.  Another thing to watch out for is that successive
lines of text should be separated by the "\r\n" characters, not just
"\n".

The value of charmap is one of "Unicode", "Shift_JIS" and "Big5".  The
interaction between Perl, Unicode and libgd is not clear to me, and
you should experiment a bit if you want to use this feature.

  Example:

    $gd->stringFT($black,'/dosc/windows/Fonts/pala.ttf',40,0,20,90,
                  "hi there\r\nbye now",
                  {linespacing=>0.6,
                   charmap  => 'Unicode',
                  });

  For backward compatibility with older versions of the FreeType
  library, the alias stringTTF() is also recognized.  Also be aware
  that relative font paths are not recognized due to problems in the
  libgd library.

=back

=head2 Alpha Channels

=over 4

=item $image->alphaBlending($blending)

NOT YET IMPLEMENTED

=item $image->saveAlpha($saveAlpha)

NOT YET IMPLEMENTED

=back

=head2 Miscellaneous Image Methods

=over 4

=item $image->interlaced([$flag])

NOT SUPPORTED

=item ($width,$height) = $image->getBounds()

getBounds() returns the height and width of the image.

=item $is_truecolor = $image->isTrueColor()

NOT YET IMPLEMENTED

=item $flag = $image1->compare($image2)

NOT YET IMPLEMENTED

=item $image->clip($x1,$y1,$x2,$y2)
       ($x1,$y1,$x2,$y2) = $image->clip

NOT YET IMPLEMENTED

=item $flag = $image->boundsSafe($x,$y)

NOT YET IMPLEMENTED

=back

=head2 Polygons

SVG is much more adept at creating polygons than GD. That said, GD
does provide some rudimentary support for polygons but must be created
as seperate objects point by point.

=over 4

=item $poly = GD::SVG::Polygon->new

Create an empty polygon with no vertices.

     $poly = new GD::SVG::Polygon;

=item $poly->addPt($x,$y)

Add point (x,y) to the polygon.

      $poly->addPt(0,0);
      $poly->addPt(0,50);
      $poly->addPt(25,25);

=item ($x,$y) = $poly->getPt($index)

Retrieve the point at the specified vertex.

     ($x,$y) = $poly->getPt(2);

=item $poly->setPt($index,$x,$y)

Change the value of an already existing vertex.  It is an error to set
a vertex that isn't already defined.

     poly->setPt(2,100,100);

=item ($x,$y) = $poly->deletePt($index)

     Delete the specified vertex, returning its value.

     ($x,$y) = $poly->deletePt(1);

=item $poly->toPt($dx,$dy)

Draw from current vertex to a new vertex, using relative (dx,dy)
coordinates.  If this is the first point, act like addPt().

      $poly->addPt(0,0);
      $poly->toPt(0,50);
      $poly->toPt(25,-25);

NOT YET IMPLEMENTED

=item $vertex_count = $poly->length

Return the number of vertices in the polygon.

   $points = $poly->length;

=item @vertices = $poly->vertices

Return a list of all the verticies in the polygon object.  Each mem-
ber of the list is a reference to an (x,y) array.

      @vertices = $poly->vertices;
      foreach $v (@vertices)
          print join(",",@$v),"\n";
      }

=item @rect = $poly->bounds

Return the smallest rectangle that completely encloses the polygon.
The return value is an array containing the (left,top,right,bottom) of
the rectangle.

      ($left,$top,$right,$bottom) = $poly->bounds;

NOT YET IMPLEMENTED

=item $poly->offset($dx,$dy)

Offset all the vertices of the polygon by the specified horizontal
(dh) and vertical (dy) amounts.  Positive numbers move the polygon
down and to the right. Returns the number of vertices affected.

      $poly->offset(10,30);

=item $poly->map($srcL,$srcT,$srcR,$srcB,$destL,$dstT,$dstR,$dstB)

Map the polygon from a source rectangle to an equivalent position in a
destination rectangle, moving it and resizing it as necessary.  See
polys.pl for an example of how this works.  Both the source and
destination rectangles are given in (left,top,right,bottom) coordi-
nates.  For convenience, you can use the polygon's own bounding box as
the source rectangle.

       # Make the polygon really tall
       $poly->map($poly->bounds,0,0,50,200);

NOT YET IMPLEMENTED

=item $poly->scale($sx,$sy)

Scale each vertex of the polygon by the X and Y factors indicated by
sx and sy.  For example scale(2,2) will make the polygon twice as
large.  For best results, move the center of the polygon to position
(0,0) before you scale, then move it back to its previous position.

NOT YET IMPLEMENTED

=item $poly->transform($sx,$rx,$sy,$ry,$tx,$ty) 

Run each vertex of the polygon through a transformation matrix, where
sx and sy are the X and Y scaling factors, rx and ry are the X and Y
rotation factors, and tx and ty are X and Y offsets.  See the Adobe
PostScript Reference, page 154 for a full explanation, or experiment.

NOT YET IMPLEMENTED

=back

=head2 GD::Polyline

Please see GD::Polyline for information on creating open polygons and
splines.

=head2 Font Utilites

NOTE: The object-oriented implementation to font utilites is not yet
supported.

The libgd library (used by the Perl GD library) has built-in support
for about half a dozen fonts, which were converted from public-domain
X Windows fonts.  For more fonts, compile libgd with TrueType support
and use the stringFT() call.

GD::SVG replicates the internal fonts of GD by hardcoding fonts which
resemble the design and point size of the original.  Each of these
fonts is available both as an imported global (e.g. gdSmallFont) and
as a package method (e.g. GD::Font->Small).

=over 4

=item gdSmallFont 
=item GD::Font->Small

This is the basic small font, "borrowed" from a well known public
domain 6x12 font.

=item gdLargeFont
=item GD::Font->Large

This is the basic large font, "borrowed" from a well known public
domain 8x16 font.

=item gdMediumBoldFont
=item GD::Font->MediumBold

This is a bold font intermediate in size between the small and large
fonts, borrowed from a public domain 7x13 font;

=item gdTinyFont
=item GD::Font->Tiny

This is a tiny, almost unreadable font, 5x8 pixels wide.

=item gdGiantFont
=item GD::Font->Giant

This is a 9x15 bold font converted by Jan Pazdziora from a sans serif
X11 font.

=item $font->nchars

This returns the number of characters in the font.

  print "The large font contains ",gdLargeFont->nchars," characters\n";

NOT YET IMPLEMENTED

=item $font->offset

This returns the ASCII value of the first character in the font

=item $width = $font->width
=item $height = $font->height

These return the width and height of the font.

  ($w,$h) = (gdLargeFont->width,gdLargeFont->height);

=back

=head1 SEE ALSO

L<GD>,
L<SVG>,
L<SVG::Manual>,
L<SVG::DOM>

=head1 ACKNOWLEDGEMENTS

Lincoln Stein, my postdoctoral mentor, author of GD.pm, and all around
Perl stud. Ronan Oger, author of SVG.pm conceptualized and implemented
another wrapper around GD at about the exact same time as this module.
He also provided helpful discussions on implementing GD functions into
SVG.

=head1 IMPORTANT NOTE! GD::SVG / SVG::GD

A second module (SVG::GD), written by Ronan Oger also provides similar
functionality as this module.

=head1 AUTHOR

Todd Harris, PhD E<lt>harris@cshl.org<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Todd Harris and the Cold Spring Harbor Laboratory

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
