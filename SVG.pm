package GD::SVG;

use strict;
use Carp 'croak','carp';
use GD;
use SVG;
#use warnings;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $AUTOLOAD);
require Exporter;

#$VERSION = sprintf "0.%d%d", q$Revision: 1.21 $ =~ /(\d+)/g;
#$VERSION = sprintf "0.%02d", q$Revision: 1.21 $ =~ /(\d+)/g;
$VERSION = sprintf "0.%d", q$Revision: 1.21 $ =~ /\d\.(\d+)/g;

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


# A global debug flag which can be overriden by new()
use constant DEBUG => 0;

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
use constant GIANT_WIDTH        => 8;
use constant GIANT_WEIGHT       => 'bold';

# TEXT_KLUDGE controls the number of pixels to bump text on the
# Y-axis in order to more closely match GD output.
use constant TEXT_KLUDGE       => '2';

#########################
# END CONSTANTS - No user serviceable options below this point
#########################

# Trap GD methods that are not yet implemented in SVG.pm
sub AUTOLOAD {
  my $self = shift;
  warn "GD method $AUTOLOAD is not implemented in GD::SVG" if ($self->{debug} > 0);
}

# There must be a better way to trap these errors
sub GD::SVG::Image::_error {
  my ($self,$method) = @_;
  warn "GD method $method is not implemented in GD::SVG" if ($self->{debug} > 0);
}


#########################
# GD Constants
#########################
# Kludge - use precalculated values of cos(theta) and sin(theta)
# so that I do no have to examine quadrants

my @cosT = (qw/1024 1023 1023 1022 1021 1020 1018 1016 1014 1011 1008
1005 1001 997 993 989 984 979 973 968 962 955 949 942 935 928 920 912
904 895 886 877 868 858 848 838 828 817 806 795 784 772 760 748 736
724 711 698 685 671 658 644 630 616 601 587 572 557 542 527 512 496
480 464 448 432 416 400 383 366 350 333 316 299 282 265 247 230 212
195 177 160 142 124 107 89 71 53 35 17 0 -17 -35 -53 -71 -89 -107 -124
-142 -160 -177 -195 -212 -230 -247 -265 -282 -299 -316 -333 -350 -366
-383 -400 -416 -432 -448 -464 -480 -496 -512 -527 -542 -557 -572 -587
-601 -616 -630 -644 -658 -671 -685 -698 -711 -724 -736 -748 -760 -772
-784 -795 -806 -817 -828 -838 -848 -858 -868 -877 -886 -895 -904 -912
-920 -928 -935 -942 -949 -955 -962 -968 -973 -979 -984 -989 -993 -997
-1001 -1005 -1008 -1011 -1014 -1016 -1018 -1020 -1021 -1022 -1023
-1023 -1024 -1023 -1023 -1022 -1021 -1020 -1018 -1016 -1014 -1011
-1008 -1005 -1001 -997 -993 -989 -984 -979 -973 -968 -962 -955 -949
-942 -935 -928 -920 -912 -904 -895 -886 -877 -868 -858 -848 -838 -828
-817 -806 -795 -784 -772 -760 -748 -736 -724 -711 -698 -685 -671 -658
-644 -630 -616 -601 -587 -572 -557 -542 -527 -512 -496 -480 -464 -448
-432 -416 -400 -383 -366 -350 -333 -316 -299 -282 -265 -247 -230 -212
-195 -177 -160 -142 -124 -107 -89 -71 -53 -35 -17 0 17 35 53 71 89 107
124 142 160 177 195 212 230 247 265 282 299 316 333 350 366 383 400
416 432 448 464 480 496 512 527 542 557 572 587 601 616 630 644 658
671 685 698 711 724 736 748 760 772 784 795 806 817 828 838 848 858
868 877 886 895 904 912 920 928 935 942 949 955 962 968 973 979 984
989 993 997 1001 1005 1008 1011 1014 1016 1018 1020 1021 1022 1023
1023/);

my @sinT = (qw/0 17 35 53 71 89 107 124 142 160 177 195 212 230 247
265 282 299 316 333 350 366 383 400 416 432 448 464 480 496 512 527
542 557 572 587 601 616 630 644 658 671 685 698 711 724 736 748 760
772 784 795 806 817 828 838 848 858 868 877 886 895 904 912 920 928
935 942 949 955 962 968 973 979 984 989 993 997 1001 1005 1008 1011
1014 1016 1018 1020 1021 1022 1023 1023 1024 1023 1023 1022 1021 1020
1018 1016 1014 1011 1008 1005 1001 997 993 989 984 979 973 968 962 955
949 942 935 928 920 912 904 895 886 877 868 858 848 838 828 817 806
795 784 772 760 748 736 724 711 698 685 671 658 644 630 616 601 587
572 557 542 527 512 496 480 464 448 432 416 400 383 366 350 333 316
299 282 265 247 230 212 195 177 160 142 124 107 89 71 53 35 17 0 -17
-35 -53 -71 -89 -107 -124 -142 -160 -177 -195 -212 -230 -247 -265 -282
-299 -316 -333 -350 -366 -383 -400 -416 -432 -448 -464 -480 -496 -512
-527 -542 -557 -572 -587 -601 -616 -630 -644 -658 -671 -685 -698 -711
-724 -736 -748 -760 -772 -784 -795 -806 -817 -828 -838 -848 -858 -868
-877 -886 -895 -904 -912 -920 -928 -935 -942 -949 -955 -962 -968 -973
-979 -984 -989 -993 -997 -1001 -1005 -1008 -1011 -1014 -1016 -1018
-1020 -1021 -1022 -1023 -1023 -1024 -1023 -1023 -1022 -1021 -1020
-1018 -1016 -1014 -1011 -1008 -1005 -1001 -997 -993 -989 -984 -979
-973 -968 -962 -955 -949 -942 -935 -928 -920 -912 -904 -895 -886 -877
-868 -858 -848 -838 -828 -817 -806 -795 -784 -772 -760 -748 -736 -724
-711 -698 -685 -671 -658 -644 -630 -616 -601 -587 -572 -557 -542 -527
-512 -496 -480 -464 -448 -432 -416 -400 -383 -366 -350 -333 -316 -299
-282 -265 -247 -230 -212 -195 -177 -160 -142 -124 -107 -89 -71 -53 -35
-17 /);


#############################
# GD::SVG::Image methods
#############################
sub GD::SVG::Image::new {
  my ($self,$width,$height,$debug) = @_;
  my $this = bless {},$self;
  my $img = SVG->new(width=>$width,height=>$height);
  $this->{img} = $img;
  $this->{width} = $width;
  $this->{height} = $height;

  # Let's create an internal representation of the image in GD
  # so that I can easily use some of GD's methods
  $this->{gd} = GD::Image->new($width,$height);

  # Let's just assume that we always want the foreground color to be
  # black This, for the most part, works for Bio::Graphics This
  # certainly needs to be fixed...
  $this->{foreground} = $this->colorAllocate('0','0','0');
  $this->{debug} = ($debug) ? $debug : DEBUG;
  return $this;
}


#############################
# Image Data Output Methods #
#############################
sub GD::SVG::Image::svg {
  my $self = shift;
  my $img = $self->{img};
  $img->xmlify(-pubid => "-//W3C//DTD SVG 1.0//EN",
               -inline => 1);
}

###sub GD::SVG::Image::png {
###  my ($self,$compression) = @_;
###  return $self->{gd}->png($compression);
###}

###sub GD::SVG::Image::jpeg {
###  my ($self,$quality) = @_;
###  return $self->{gd}->jpeg($quality);
###}

#############################
# Color management routines #
#############################
sub GD::SVG::Image::colorAllocate {
  my ($self,$r,$g,$b) = @_;
  $r ||= 0;
  $g ||= 0;
  $b ||= 0;
  my $color = "rgb($r,$g,$b)";
  my $index = $self->{gd}->colorAllocate($r,$g,$b);

  # This storage of colors is redundant and needs to be cleaned up
  # Save GDs color_index, keyed by the RGB triplet that SVG utilizes.
  # As well as the raw rgb triplet...
  $self->{color_index}->{$color} = [$index,$r,$g,$b];
  # Keep a list of colors in ther order that they are added as well
  push (@{$self->{colors_added}},$color);
  return $color;
}

sub GD::SVG::Image::colorAllocateAlpha {
  my ($self,$r,$g,$b,$alpha) = @_;
  ### $self->{gd}->colorAllocateAlpha($r,$g,$b,$alpha);
  $self->_error('colorAllocateAlpha');
}

sub GD::SVG::Image::colorDeallocate {
  my ($self,$index) = @_;
  my $colors = %{$self->{color_index}};
  delete $colors->{$index};
  ### $self->{gd}->colorDeallocate($index);
}

# workaround for bad GD
sub GD::SVG::Image::colorClosest {
  my ($self,@c) = @_;
  ### my $index = $self->{gd}->colorClosest(@c);

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

sub GD::SVG::Image::colorClosestHWB { shift->_error('colorClosestHWB'); }

sub GD::SVG::Image::colorExact {
  my ($self,$r,$g,$b) = @_;
  ### my $index = $self->{gd}->colorExact($r,$g,$b);
  
  # Let's just allocate the color instead of looking it up
  my $color = $self->colorAllocate($r,$g,$b);
  if ($color) {
    return $color;
  } else {
    return ('-1');
  }
}

sub GD::SVG::Image::colorResolve {
  my ($self,$r,$g,$b) = @_;
  ### my $index = $self->{gd}->colorResolve($r,$g,$b);
  my $color = $self->colorAllocate($r,$g,$b);
  return $color;
}

sub GD::SVG::Image::colorsTotal {
  my $self = shift;
  return $self->{gd}->colorsTotal;
}


# NOT YET IMPLEMENTED
sub GD::SVG::Image::getPixel {
  my ($self,$x,$y) = @_;
  # Internal GD - probbaly unnecessary in this context...
  # Will contstruct appropriate return value later
  #  $self->{gd}->getPixel($x,$y);
  
  # I don't have any cogent way to fetch the value of an asigned pixel
  # without calculating all positions and loading into memory.
  
  # For these purposes, I could maybe just look it up...  From a hash
  # table or something - Keep track of all assigned pixels and their
  # color...
  return ('rgb(255,255,255)');
}

# Given the color index, return its rgb index...  GD::SVG does not
# pass indices, instead using stringified RGB triplets. This makes it
# simple to return the rgb triplet from the "index" value...
sub GD::SVG::Image::rgb {
  my ($self,$index) = @_;
  $index =~ /rgb\((.*),(.*),(.*)\)/;
  my $r = $1;
  my $g = $2;
  my $b = $3;
  return ($r,$g,$b);

  # This is a more in depth approach that allows me to maintain the state
  # of the internal GD object as well.
  
  # Invert the internal color index hash
  #  my %indices = map { $self->{color_index}->{$_} => $_ } keys %{$self->{color_index}};
  #  my $color = $indices{$index};
  #  return $color;
}

sub GD::SVG::Image::transparent { shift->_error('transparent'); }

#######################
# Special Colors
#######################
# Kludgy preliminary support for gdBrushed This is based on
# Bio::Graphics implementation of set_pen which in essence just
# controls line color and thickness...  We will assume that the last
# color added is intended to be the foreground color.
sub GD::SVG::Image::setBrush {
  my ($self,$pen) = @_;
  ### $self->{gd}->setBrush($pen);

  my ($width,$height) = $pen->getBounds();
  my $last_color = $pen->{colors_added}->[-1];
  my ($r,$g,$b) = $self->rgb($last_color);
  $self->{gdBrushed}->{fg} = $self->colorAllocate($r,$g,$b);
  $self->{gdBrushed}->{thickness} = $width;
}

# There is no direct translation of gdStyled.  In gd, this is used to
# set the style for the line usiing the settings of the current brush.
# Drawing with the new style is then used by passing the gdStyled as a
# color.
sub GD::SVG::Image::setStyle {
  my ($self,@params) = @_;
  ### $self->{gd}->setStyle(@params);
  $self->{current_style} = [ @params ];
  return;
}

# Lines in GD are 1 pixel in diameter by default.
# setThickness allows line thickness to be changed.
# This should be retained until it's changed again
# Each method should check the thickness of the line...
sub GD::SVG::Image::setThickness {
  my ($self,$thickness) = @_;
  ### $self->{gd}->setThickness($thickness);
  $self->{line_thickness} = $thickness;
  # $self->{prev_line_thickness} = (!defined $self->{prev_line_thickness}) ? $thickness : undef;
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
sub gdStyled  { return 'gdStyled'; }
sub gdBrushed { return 'gdBrushed'; }

sub gdStyledBrush { _error('gdStyledBrush'); }
sub gdTiled       { _error('gdTiled'); }
sub gdDashSize    { _error('gdDashSize'); }
sub gdMaxColors   { _error('gdMaxColors'); }

sub gdTransparent { return 'transparent'; }

sub gdAntiAliased { _error('gdAntiAliased'); }
sub GD::SVG::Image::setAntiAliased { shift->_error('setAntiAliased'); }
sub GD::SVG::Image::setAntiAliasedDontBlend { shift->_error('setAntiAliasedDontBlend'); }

#######################
# Drawing subroutines #
#######################
sub GD::SVG::Image::setPixel {
  my ($self,$x1,$y1,$color,$color2) = @_;
  ### $self->{gd}->setPixel($x1,$y1,$color);

  my $img = $self->{img};
  my $id = $self->_create_id($x1,$y1);
  my $result =
    $img->circle(cx=>$x1,cy=>$y1,r=>'0.03',
		 id=>$id,
		 style=>{
			 'stroke'=>$color,
			 'fill'=>$color,
			 # 'stroke-opacity'=>'0.5',
			 'fill-opacity'=>'1.0'
			}
		);
  return $result;
}

sub GD::SVG::Image::line {
  my ($self,$x1,$y1,$x2,$y2,$color) = @_;
  # Are we trying to draw with a styled line (ie gdStyled, gdBrushed?)
  # If so, we need to deconstruct the values for line thickness and
  # foreground color
  # This needs to be generalized for gdStyled as well
  if ($color eq 'gdStyled' || $color eq 'gdBrushed') {
    my $fg = $self->_distill_gdBrushed($color);
    $self->line($x1,$y1,$x2,$y2,$fg);
  } else {
    ### $self->{gd}->line($x1,$y1,$x2,$y2,$color);
    my $img = $self->{img};
    my $id = $self->_create_id($x1,$y1);
    my $thickness = $self->_get_thickness() || 1;
    my $result = $img->line(x1=>$x1,y1=>$y1,
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
    $self->_reset();
    return $result;
  }
}


sub GD::SVG::Image::dashedLine { shift->_error('dashedLine'); }

# The fill parameter is used internally as a simplification...
sub GD::SVG::Image::rectangle {
  my ($self,$x1,$y1,$x2,$y2,$color,$fill) = @_;
  if ($color eq 'gdStyled' || $color eq 'gdBrushed') {
    my $fg = $self->_distill_gdBrushed($color);
    $self->rectangle($x1,$y1,$x2,$y2,$fg,$fill);
  } else {
    ### $self->{gd}->rectangle($x1,$y1,$x2,$y2,$color);
    my $img = $self->{img};
    my $id = $self->_create_id($x1,$y1);
    my $foreground = $self->{foreground};
    my $fg = ($color eq $foreground) ? $foreground : $color;
    my $thickness = $self->_get_thickness() || 1;

    my $fill_opacity = ($fill) ? 1 : 0;
    $fill ||= 'none';
    my $result = 
      $img->rectangle(x=>$x1,y=>$y1,
		      width  =>$x2-$x1,
		      height =>$y2-$y1,
		      id     =>$id,
		      style  => {
				 'stroke'         => $color,
				 'stroke-width'   => $thickness,
				 'stroke-opacity' => '1',
				 'fill-opacity'   => $fill_opacity,
				 'fill'           => $fill,
				}
		     );
    $self->_reset();
    return $result;
  }
}

# This should just call the rectangle method passing it a flag.
# I will need to fix the glyph that bypasses this option...
sub GD::SVG::Image::filledRectangle {
  my ($self,$x1,$y1,$x2,$y2,$color) = @_;
  # Call the rectangle method passing the fill color
  $self->rectangle($x1,$y1,$x2,$y2,$color,$color);
}

sub GD::SVG::Image::polygon {
  my ($self,$poly,$color,$fill) = @_;
  if ($color eq 'gdStyled' || $color eq 'gdBrushed') {
    my $fg = $self->_distill_gdBrushed($color);
    $self->polygon($poly,$fg);
  } else {
    ### $self->{gd}->polygon($poly,$color);
    my $img = $self->{img};
    # Create seperate x and y arrays of vertices
    my @xpoints = $poly->_fetch_vertices('x');
    my @ypoints = $poly->_fetch_vertices('y');
    my $points = $img->get_path(
				x=>\@xpoints, y=>\@ypoints,
				-type=>'polygon'
			       );
    my $id = $self->_create_id($xpoints[0],$ypoints[0]);
    my $thickness = $self->_get_thickness() || 1;
    my $fill_opacity = ($fill) ? 1 : 0;
    $fill ||= 'none';
    my $result =
      $img->polygon(
		    %$points,
		    id=>$id,
		    style=>{
			    'stroke'        => $color,
			    'stroke-opacity'=> '1.0',
			    'stroke-width'  => $thickness,			
			    'fill'          => $fill,
			    'fill-opacity'  => $fill_opacity,
			   }
		   );
    $self->_reset();
    return $result;
  }
}

# Passing the stroke doesn't really work as expected...
sub GD::SVG::Image::filledPolygon {
  my ($self,$poly,$color,$stroke) = @_;
  my $result = $self->polygon($poly,$color,$color);
  return $result;
}

sub GD::SVG::Image::ellipse {
  my ($self,$x1,$y1,$width,$height,$color,$fill) = @_;
  if ($color eq 'gdStyled' || $color eq 'gdBrushed') {
    my $fg = $self->_distill_gdBrushed($color);
    $self->ellipse($x1,$y1,$width,$height,$fg);
  } else {
    ### $self->{gd}->ellipse($x1,$y1,$width,$height,$color);
    my $img = $self->{img};
    my $id = $self->_create_id($x1,$y1);
    # GD uses width and height - SVG uses radii...
    $width  = $width / 2;
    $height = $height / 2;
    my $thickness = $self->_get_thickness() || 1;
    my $fill_opacity = ($fill) ? '1' : '0';
    $fill ||= 'none';
    my $result =
      $img->ellipse(
		    cx=>$x1, cy=>$y1,
		    rx=>$width, ry=>$height,
		    id=>$id,
		    style=>{
			    'stroke'        =>$color,
			    'stroke-width'  => $thickness,
			    'stroke-opacity'=>'1.0',
			    'fill'          => $fill,
			    'fill-opacity'  =>$fill_opacity,
			   }
		   );
    $self->_reset();
    return $result;
  }
}

sub GD::SVG::Image::filledEllipse {
  my ($self,$x1,$y1,$width,$height,$color) = @_;
  my $result = $self->ellipse($x1,$y1,$width,$height,$color,$color);
  return $result;
}

# GD uses the arc() and filledArc() methods in two capacities
#   1. to create closed ellipses, where start and end are 0 and 360
#   2. to create honest-to-god open arcs
sub GD::SVG::Image::arc {
  my ($self,$cx,$cy,$width,$height,$start,$end,$color,$fill) = @_;
  if ($color eq 'gdStyled' || $color eq 'gdBrushed') {
    my $fg = $self->_distill_gdBrushed($color);
    $self->arc($cx,$cy,$width,$height,$start,$end,$fg);
  } else {
    ### $self->{gd}->arc($x,$y,$width,$height,$start,$end,$color);
    # Are we just trying to draw a closed arc (an ellipse)?
    my $result;
    if ($start eq '0' and $end eq '360') {
      $result = $self->ellipse($cx,$cy,$width,$height,$color,$fill);
    } else {
      my $img = $self->{img};
      my $id = $self->_create_id($cx,$cy);
      my $thickness = $self->_get_thickness() || 1;

      # Taking a stab at drawing elliptical arcs
      # GD uses diameters, SVG uses radii
      my $a = $width  / 2;
      my $b = $height / 2;

      while ($start < 0 )    { $start += 360; }
      while ($end < 0 )      { $end   += 360; }
      while ($end < $start ) { $end   += 360; }
      my ($startx,$starty) = _calculate_arc_coords($cx,$cy,$width,$height,$start);
      my ($endx,$endy)     = _calculate_arc_coords($cx,$cy,$width,$height,$end);

      # M = move to (origin of the curve)
      # my $rotation = abs $start - $end;
      my $large = (abs $start - $end > 180) ? 1 : 0;
      # my $sweep = ($start > $end) ? 0 : 1;  # directionality of the arc, + CW, - CCW
      my $sweep = 1; # Always CW with GD
      my $fill_opacity = ($fill) ? 1 : 0;
      $fill ||= 'none';
      $result =
      	$img->path('d'=>"M$startx,$starty "  .
      		   "A$a,$b 0 $large,$sweep $endx,$endy",
		   style=>{
			   'stroke'      => $color,
			   'stroke-width'=> $thickness,
			   'fill'        => $fill,
			   'fill-opacity'=> $fill_opacity,
			  },
		  );

      # Now I need to draw a filled triangle to complete the filledArc
      if ($fill ne 'none') {
	my $poly = GD::SVG::Polygon->new();
	$poly->addPt($cx,$cy);
	$poly->addPt($startx,$starty);
	$poly->addPt($endx,$endy);
	$self->filledPolygon($poly,$color);
      }
    }
    $self->_reset();
    return $result;
  }
}

# Return the x and y positions of start and stop of arcs.
sub _calculate_arc_coords {
  my ($cx,$cy,$width,$height,$angle) = @_;
  my $x = ( $cosT[$angle % 360] * $width)  / (2 * 1024) + $cx;
  my $y = ( $sinT[$angle % 360] * $height) / (2 * 1024) + $cy;
  return ($x,$y);
}

# filledArc needs to attach a filled triangle to the filledArc in
# order to complete it
sub GD::SVG::Image::filledArc {
  my ($self,$cx,$cy,$width,$height,$start,$end,$color) = @_;
  my $result = $self->arc($cx,$cy,$width,$height,$start,$end,$color,$color);
  return $result;
}

# Flood fill that stops at first pixel of a different color.
sub GD::SVG::Image::fill         { shift->_error('fill'); }
sub GD::SVG::Image::fillToBorder { shift->_error('fillToBorder'); }

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
  my $result =
    $img->text(
	       id=>$id,
	       x=>$x,
	       y=>$y + $font_obj->{height} - TEXT_KLUDGE,
	       %$formatting,
	       transform => $transform,
	       'fill'    =>$color,
	      )->cdata($text);
  return $result;
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
#sub GD::Image::stringFT { shift->_error('stringFT'); }


##################################################
# Alpha Channels
##################################################
sub GD::SVG::Image::alphaBlending { shift->_error('alphaBlending'); }
sub GD::SVG::Image::saveAlpha     { shift->_error('saveAlpha'); }

##################################################
# Miscellaneous Image Methods
##################################################
sub GD::SVG::Image::interlaced { shift->_error('inerlaced'); }

sub GD::SVG::Image::getBounds {
  my $self = shift;
  my $width = $self->{width};
  my $height = $self->{height};
  return($width,$height);
}

sub GD::SVG::Image::isTrueColor { shift->_error('isTrueColor'); }
sub GD::SVG::Image::compare     { shift->_error('compare'); }
sub GD::SVG::Image::clip        { shift->_error('clip'); }
sub GD::SVG::Image::boundsSafe  { shift->_error('boundsSafe'); }


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

# Break apart the internal representation of gdBrushed
# setting the line thickness and returning the foreground color
sub GD::SVG::Image::_distill_gdBrushed {
  my ($self,$type) = @_;
  # Save the previous line thickness so I can restore after drawing...
  $self->{prev_line_thickness} = $self->_get_thickness() || 1;
  my $thickness = $self->{$type}->{thickness};
  my $fg = $self->{$type}->{fg};
  $self->setThickness($thickness);
  return $fg;
}


# Reset presistent drawing settings between uses of stylized brushes
sub GD::SVG::Image::_reset {
  my $self = shift;
  $self->{line_thickness} = $self->{prev_line_thickness} || $self->{line_thickness};
  $self->{prev_line_thickness} = undef;
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

# Value-access methods
# Get the thickness of the line (if it has been set)
sub GD::SVG::Image::_get_thickness {  return shift->{line_thickness} }

# return the internal GD object
sub _gd { return shift->{gd} }


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

# return an array of x or y vertices...
sub _fetch_vertices {
  my ($poly,$flag) = @_;
  my @vertices = $poly->vertices;
  my $axis = ($flag eq 'x') ? 0 : 1;
  my @points;
  foreach my $vertex (@vertices) {
    push (@points,$vertex->[$axis]);
  }
  return @points;
}

sub bounds {
  my $self = shift;
  my @vertices = $self->vertices;
  my @x = $self->_fetch_vertices('x');
  my @y = $self->_fetch_vertices('y');
  @x = sort { $a <=> $b } @x;
  @y = sort { $a <=> $b } @y;
  my $left   = $x[0];
  my $right  = $x[-1];
  my $top    = $y[-1];
  my $bottom = $y[0];
  return ($left,$top,$right,$bottom);
}

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

sub map       { shift->_error('GD::SVG::Polygon::map'); }
sub transform { shift->_error('GD::SVG::Polygon::transform'); }

sub _error {
  my ($self,$method) = @_;
  GD::SVG::Image->_error($method);
}

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
sub nchars  { shift->_error('nchars')} # NOT SUPPORTED!!

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

sub _error {
  my ($self,$method) = @_;
  GD::SVG::Image->_error($method);
}

sub DESTROY { }


1;

=pod

=head1 NAME

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

GD::SVG exports the same methods as GD itself, overriding those
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

=head1 DYNAMICALLY SELECTING SVG OUTPUT

If you would like your script to be able to dynamically select either
PNG or JPEG output (via GD) or SVG output (via GD::SVG), you should
place your "use" statement within an eval. In the example below, each
of the available classes is created at the top of the script for
convenience, as well as the image output type.

  my $package = shift;
  eval "use $package";
  my $image_pkg = $package . '::Image';
  my $poly_pkg  = $package . '::Polygon';
  my $font_pkg  = $package . '::Font';
    
  # Creating new images or polygons thus becomes
  my $image   = $image_pkg->new($width,$height);
  my $polygon = $poly_pkg->new();

  # Establish the image output type
  my $image_type;
  if ($package = 'GD::SVG') {
    $image_type = 'svg';
  } else {
    $image_type = 'png';
  }

Finally, you should change all GD::Image, GD::Font, and GD::Polygon
references to $image_pkg->, $font_pkg->, $poly_pkg->, respectively.

  GD::Image->new()   becomes   $image_pkg->new()
  GD::Font->Large()  becomes   $font_pkg->Large()
  GD::Polygon->new() becomes   $poly_pkg->new()

If you make heavy use of GD's exported methods, it may also be
necessary to add () to the endo of method names to avoide bareword
compilation errors. That's the price you pay for using exported
functions!

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

A similar module (SVG::GD) implements a similar wrapper around
GD. Please see the section at the bottom of this document that
compares GD::SVG to SVG::GD.

=head1 PREREQUISITES

GD::SVG requires the Ronan Oger's SVG.pm module, Lincoln Stein's GD.pm
module, libgd and its dependencies.

=head1 GENERAL DIFFICULTIES IN TRANSLATING GD TO SVG

These are the primary weaknesses of GD::SVG.

=over 4

=item SVG requires unique identifiers for each element

Each element in an SVG image requires a unique identifier. In general,
GD::SVG handles this by automatically generating unique random
numbers.  In addition to the typical parameters for GD methods,
GD::SVG methods allow a user to pass an optional id parameter for
naming the object.

=item Direct calls to the GD package will fail

You must change direct calls to the classes that GD invokes:
    GD::Image->new() should be changed to GD::SVG::Image->new()

See the documentation above for how to dynamically switch between
packages.

=item raster fill() and fillToBorder() not supported

As SVG documents are not inherently aware of their canvas, the flood
fill methods are not currently supported.n

=item getPixel() not supported.

Although setPixel() works as expected, its counterpart getPixel() is
not supported. I plan to support this method in a future release.

=item arc() and filledArc() partially supported

Currently, GD::SVG supports the drawing of closed (ie full 360 degree
arcs) and open arcs, but does not support the special arc styling
methods such as gdChord and gdArc.

=item Images can only be created dynamically, not from pre-existing images

GD::SVG does not currently support the newFrom()
methods. Consequently, it can only be used for the dynamic creation of
images.

=item No support for generation of images from filehandles or raw data

GD::SVG works only with scripts that generate images directly in the
code using the GD->new(height,width) approach.

=item Tiled fills are not supported

Any functions passed gdTiled objects will die.

=item Styled and Brushed lines only partially implemented

Calls to the gdStyled, gdTransparent, and gdBrushed functions via a
rather humorous kludge (and simplification). Depending on the
complexity of the brush, they may behave from slightly differently to
radically differently from their behavior under GD. You have been
warned. See the documentation sections for the methods that set these
options (setStyle(), setBrush(), and setTransparent()).

=back

See below for a full list of methods that have not yet been
implemented.

=head1 WHEN THINGS GO WRONG

GD is a complicated module.  Translating GD methods into those
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

=head1 GD FUNCTIONS MISSING FROM GD::SVG

The following GD functions have not yet been incorporated into
GD::SVG. If you attempt to use one of these functions (and you have
enabled debug warnings via the new() method), GD::SVG will print a
warning to STDERR.

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

  Color control methods:
    $image->colorAllocateAlpha()
    $image->colorClosest()
    $image->colorClosestHWB()
    $image->getPixel()
    $image->transparent()

  Special Colors:
    $image->setBrush() (semi-supported, with kludge)
    $image->setStyle()
    gdTiled
    gdStyled
    $image->setAntialiased()
    gdAntiAliased()
    $image->setAntiAliasedDontBlend()

  Drawing methods:
    $image->dashedLine()
    $image->fill()
    $image->fillToBorder()

  Image copying methods
    None of the image copying methods are yet supported

  Image transformation methods
    None of the image transformation methods are yet supported

  Character and string drawing methods
     $image->stringUp()  - incompletely supported - broken
     $image->charUp()
     $image->stringFT()

  Alpha Channels
    $image->alphaBlending()
    $image->saveAlpha()

  Miscellaneous image methods
    $image->isTrueColor()
    $image->compare($image2)
    $image->clip()
    $image->boundsSafe()

  Polygon methods:
    $polygon->toPt(x,y)
    $polygon->bounds()
    $polygon->map()
    $polygon->scale()
    $polygon->transform()

  GD::Polyline
    Currently no support for GD::Polyline

  Font methods:
    $font->nchars()
    $font->offset()

=head1 GD VERSUS GD::SVG METHODS

All GD::SVG methods mimic the naming and interface of GD methods.  As
such, maintenance of GD::SVG follows the development of both GD and
SVG. Much of the original GD documentation is replicated here for ease
of use. Subtle differences in the implementation of these methods
between GD and GD::SVG are discussed below. In particular, the return
value for some GD::SVG methods differs from its GD counterpart.

=head1 OBJECT CONSTRUCTORS: CREATING IMAGES

GD::SVG currently only supports the creation of image objects via its
new constructor.  This is in contrast to GD proper which supports the
creation of images from previous images, filehandles, filenames, and
data.

=over 4

=item $image = GD::SVG::Image->new($height,$width,$debug);

Create a blank GD::SVG image object of the specified dimensions in
pixels. In turn, this method will create a new SVG object and store it
internally. You can turn on debugging with the GD::SVG specific $debug
parameter.  This should be boolean true and will cause non-implemented
methods to print a warning on their status to STDERR.

=back

=head1 GD::SVG::Image METHODS

Once a GD::Image object is created, you can draw with it, copy it, and
merge two images.  When you are finished manipulating the object, you
can convert it into a standard image file format to output or save to
a file.

=head2 Image Data Output Methods

GD::SVG implements a single output method, svg()!

=over 4

=item $svg = $image->svg();

This returns the image in SVG format. You may then print it, pipe it
to an image viewer, or write it to a file handle. For example,

  $svg_data = $image->svg();
  open (DISPLAY,"| display -") || die;
  binmode DISPLAY;
  print DISPLAY $svg_data;
  close DISPLAY;

if you'd like to return an inline version of the image (instead of a
full document version complete with the DTD), pass the svg() method the
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

=item $index = $image->colorAllocateAlpha()

NOT IMPLEMENTED

=item $image->colorDeallocate($index)

Provided with a color index, remove it from the color table.

=item $index = $image->colorClosest(red,green,blue)

This returns the index of the color closest in the color table to the
red green and blue components specified. This method is inherited
directly from GD.

  Example: $apricot = $myImage->colorClosest(255,200,180);

NOT IMPLEMENTED

=item $index = $image->colorClosestHWB(red,green,blue)

NOT IMPLEMENTED

=item $index = $image->colorExact(red,green,blue)

Retrieve the color index of an rgb triplet (or -1 if it has yet to be
allocated).

NOT IMPLEMENTED

=item $index = $image->colorResolve(red,green,blue)

NOT IMPLEMENTED

=item $colors_total = $image->colorsTotal()

Retrieve the total number of colors indexed in the image.

=item $index = $image->getPixel(x,y)

NOT IMPLEMENTED

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
loaded. GD::SVG offers limited support for these methods.

=over 4

=item $image->setBrush($brush) (KLUDGE ALERT)

=item gdBrushed

In GD, one can draw lines and shapes using a brush pattern.  Brushes
are just images that you can create and manipulate in the usual way.
When you draw with them, their contents are used for the color and
shape of the lines.

To make a brushed line, you must create or load the brush first, then
assign it to the image using setBrush().  You can then draw in that
with that brush using the gdBrushed special color.  It's often useful
to set the background of the brush to transparent so that the
non-colored parts don't overwrite other parts of your image.

  # Via GD, this is how one would set a Brush
  $diagonal_brush = new GD::Image(5,5);
  $white = $diagonal_brush->colorAllocate(255,255,255);
  $black = $diagonal_brush->colorAllocate(0,0,0);
  $diagonal_brush->transparent($white);
  $diagonal_brush->line(0,4,4,0,$black); # NE diagonal

GD::SVG offers limited support for setBrush (and the corresponding
gdBrushed methods) - currently only in the shapes of squares.
Internally, GD::SVG extracts the longest dimension of the image using
the getBounds() method. Next, it extracts the second color set,
assuming that to be the foreground color. It then re-calls the
original drawing method with these new values in place of the
gdBrushed. See the private _distill_gdBrushed method for the internal
details of this operation.

=item $image->setThickness($thickness)

Lines drawn with line(), rectangle(), arc(), and so forth are 1 pixel
thick by default.  Call setThickness() to change the line drawing
width.

=item $image->setStyle(@colors)

NOT IMPLEMENTED

=item gdTiled

NOT IMPLEMENTED

=item gdStyled

NOT IMPLEMENTED

=item $image->setAntiAliased($color)

NOT IMPLEMENTED

=item gdAntiAliased

NOT IMPLEMENTED

=item $image->setAntiAliasedDontBlend($color,[$flag])

NOT IMPLEMENTED

=back

=head2 Drawing Commands

=over 4

=item $image->setPixel($x,$y,$color)

Set the corresponding pixel to the given color.  GD::SVG implements
this by drawing a single dot in the specified color at that position.

=item $image->line(x1,y1,x2,y2,color);

Draw a line between the two coordinate points with the specified
color.  Passing an optional id will set the id of that SVG
element. GD::SVG also supports drawing with the special brushes -
gdStyled and gdBrushed - although these special styles are difficult
to replicate precisley in GD::SVG.

=item $image->dashedLine($x1,$y1,$x2,$y2,$color);

NOT IMPLEMENTED

=item $image->rectangle($x1,$y1,$x2,$y2,$color);

This draws a rectangle with the specified color.  (x1,y1) and (x2,y2)
are the upper left and lower right corners respectively.  You may also
draw with the special colors gdBrushed and gdStyled.

=item $image->filledRectangle($x1,$y1,$x2,$y2,$color);

filledRectangle is a GD specific method with no direct equivalent in
SVG.  GD::SVG translates this method into an SVG appropriate method by
passing the filled color parameter as a named 'filled' parameter to
SVG. Drawing with the special colors is also permitted. See the
documentation for the line() method for additional details.

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
colors gdBrushed, gdStyled and gdStyledBrushed can be specified. See
the documentation for the line() method for additional details.

  $poly = new GD::::SVG::Polygon;
  $poly->addPt(50,0);
  $poly->addPt(99,99);
  $poly->addPt(0,99);
  $image->polygon($poly,$blue);

=item $image->filledPolygon($polygon,$color);

This draws a polygon filled with the specified color.  Drawing with
the special colors is also permitted. See the documentation for the
line() method for additional details.

  # make a polygon
  $poly = new GD::SVG::Polygon;
  $poly->addPt(50,0);
  $poly->addPt(99,99);
  $poly->addPt(0,99);

  # draw the polygon, filling it with a color
  $image->filledPolygon($poly,$peachpuff);

=item $image->ellipse($cx,$cy,$width,$height,$color)

=item $image->filledEllipse($cx,$cy,$width,$height,$color)

These methods() draw ellipses. ($cx,$cy) is the center of the arc, and
($width,$height) specify the ellipse width and height, respectively.
filledEllipse() is like ellipse() except that the former produces
filled versions of the ellipse. Drawing with the special colors is
also permitted. See the documentation for the line() method for
additional details.

=item $image->arc($cy,$cy,$width,$height,$start,$end,$color);

This draws arcs and ellipses.  (cx,cy) are the center of the arc, and
(width,height) specify the width and height, respectively.  The
portion of the ellipse covered by the arc are controlled by start and
end, both of which are given in degrees from 0 to 360.  Zero is at the
top of the ellipse, and angles increase clockwise.  To specify a
complete ellipse, use 0 and 360 as the starting and ending angles.  To
draw a circle, use the same value for width and height.

Internally, arc() calls the ellipse() method of SVG.pm. Drawing with
the special colors is also permitted. See the documentation for the
line() method for additional details.

Currently, true arcs are NOT supported, only those where the start and
end equal 0 and 360 respectively resulting in a closed arc.

=item $image->filledArc($cx,$cy,$width,$height,$start,$end,$color
[,$arc_style])

This method is like arc() except that it colors in the pie wedge with
the selected color.  $arc_style is optional.  If present it is a
bitwise OR of the following constants:

gdArc           connect start & end points of arc with a rounded edge
gdChord         connect start & end points of arc with a straight line
gdPie           synonym for gdChord
gdNoFill        outline the arc or chord
gdEdged         connect beginning and ending of the arc to the center

gdArc and gdChord are mutally exclusive.  gdChord just connects the
starting and ending angles with a straight line, while gdArc pro-
duces a rounded edge. gdPie is a synonym for gdArc. gdNoFill indi-
cates that the arc or chord should be outlined, not filled.  gdEdged,
used together with gdNoFill, indicates that the beginning and ending
angles should be connected to the center; this is a good way to
outline (rather than fill) a "pie slice."

=item $image->fill();

NOT IMPLEMENTED

=item $image->fillToBorder()

NOT IMPLEMENTED

=back

=head2 Image Copying Methods

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

  $gd->stringFT($black,'/dosc/windows/Fonts/pala.ttf',40,0,20,90,
                "hi there\r\nbye now",
                {linespacing=>0.6,
                 charmap  => 'Unicode',
               });

For backward compatibility with older versions of the FreeType
library, the alias stringTTF() is also recognized.  Also be aware that
relative font paths are not recognized due to problems in the libgd
library.

=back

=head2 Alpha Channels

=over 4

=item $image->alphaBlending($blending)

NOT IMPLEMENTED

=item $image->saveAlpha($saveAlpha)

NOT IMPLEMENTED

=back

=head2 Miscellaneous Image Methods

=over 4

=item $image->interlaced([$flag])

NOT IMPLEMENTED

=item ($width,$height) = $image->getBounds()

getBounds() returns the height and width of the image.

=item $is_truecolor = $image->isTrueColor()

NOT IMPLEMENTED

=item $flag = $image1->compare($image2)

NOT IMPLEMENTED

=item $image->clip($x1,$y1,$x2,$y2)
       ($x1,$y1,$x2,$y2) = $image->clip

NOT IMPLEMENTED

=item $flag = $image->boundsSafe($x,$y)

NOT IMPLEMENTED

=back

=head1 GD::SVG::Polygon METHODS

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

  $poly->setPt(2,100,100);

=item ($x,$y) = $poly->deletePt($index)

Delete the specified vertex, returning its value.

  ($x,$y) = $poly->deletePt(1);

=item $poly->toPt($dx,$dy)

Draw from current vertex to a new vertex, using relative (dx,dy)
coordinates.  If this is the first point, act like addPt().

  $poly->addPt(0,0);
  $poly->toPt(0,50);
  $poly->toPt(25,-25);

NOT IMPLEMENTED

=item $vertex_count = $poly->length()

Return the number of vertices in the polygon.

=item @vertices = $poly->vertices()

Return a list of all the verticies in the polygon object.  Each mem-
ber of the list is a reference to an (x,y) array.

  @vertices = $poly->vertices;
  foreach $v (@vertices)
      print join(",",@$v),"\n";
  }

=item @rect = $poly->bounds()

Return the smallest rectangle that completely encloses the polygon.
The return value is an array containing the (left,top,right,bottom) of
the rectangle.

  ($left,$top,$right,$bottom) = $poly->bounds;

NOT IMPLEMENTED

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

NOT IMPLEMENTED

=item $poly->scale($sx,$sy)

Scale each vertex of the polygon by the X and Y factors indicated by
sx and sy.  For example scale(2,2) will make the polygon twice as
large.  For best results, move the center of the polygon to position
(0,0) before you scale, then move it back to its previous position.

NOT IMPLEMENTED

=item $poly->transform($sx,$rx,$sy,$ry,$tx,$ty)

Run each vertex of the polygon through a transformation matrix, where
sx and sy are the X and Y scaling factors, rx and ry are the X and Y
rotation factors, and tx and ty are X and Y offsets.  See the Adobe
PostScript Reference, page 154 for a full explanation, or experiment.

NOT IMPLEMENTED

=back

=head2 GD::Polyline

Please see GD::Polyline for information on creating open polygons and
splines.

=head1 GD::SVG::Font METHODS

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

=item gdTinyFont

=item GD::Font->Tiny

This is a tiny, almost unreadable font, 5x8 pixels wide.

=item gdSmallFont

=item GD::Font->Small

This is the basic small font, "borrowed" from a well known public
domain 6x12 font.

=item gdMediumBoldFont

=item GD::Font->MediumBold

This is a bold font intermediate in size between the small and large
fonts, borrowed from a public domain 7x13 font;

=item gdLargeFont

=item GD::Font->Large

This is the basic large font, "borrowed" from a well known public
domain 8x16 font.

=item gdGiantFont

=item GD::Font->Giant

This is a 9x15 bold font converted by Jan Pazdziora from a sans serif
X11 font.

=item $font->nchars

This returns the number of characters in the font.

  print "The large font contains ",gdLargeFont->nchars," characters\n";

NOT IMPLEMENTED

=item $font->offset()

This returns the ASCII value of the first character in the font

=item $width = $font->width

=item $height = $font->height

These return the width and height of the font.

  ($w,$h) = (gdLargeFont->width,gdLargeFont->height);

=back

=head1 REAL WORLD EXAMPLES

=over 4

=item BioPerl

The Bio::Graphics package of the BioPerl project makes use of GD::SVG
to export SVG graphics.

  http://www.bioperl.org/

=item Generic Genome Browser

The Generic Genome Browser (GBrowse) utilizes Bio::Graphics and
enables SVG dumping of genomics views. You can see a real-world
example of SVG output from GBrowse at WormBase:

  http://www.wormbase.org/cgi-bin/gbrowse/

Further information about the Generic Genome Browser is available at
the Generic Model Organism Project home page:

  http://www.gmod.org/

=item toddot

I've also prepared a number of comparative images at my website
(shameless plug, hehe):

  http://www.toddot.net/projects/GD-SVG/

=back

=head1 INTERNAL METHODS

The following internal methods are private and documented only for
those wishing to extend the GD::SVG interface.

=over 4

=item _distill_gdBrushed()

When a drawing method is passed a stylized brush via gdBrushed, the
internal _distill_gdBrushed method attempts to make sense of this by
setting line thickness and foreground color. Since stylized brushes
are GD::SVG::Image objects, it does this by fetching the width of the
image using the getBounds method. This width is then used to
setThickness.  The last color set by colorAllocate is then used for
the foreground color.

In setting line thickness, GD::SVG temporarily overrides any
previously set line thickness.  In GD, setThickness is persistent
through uses of stylized brushes. To accomodate this behavior,
_distill_gdBrushed temporarily stores the previous line_thickness in
the $self->{previous_line_thickness} flag.

=item _reset()

The _reset() method is used to restore persistent drawing settings
between uses of stylized brushes. Currently, this involves

  - restoring line thickness

=back

=head1 IMPORTANT NOTE! GD::SVG / SVG::GD

A second module (SVG::GD), written by Ronan Oger also provides similar
functionality as this module. Ronan and I are concurrently developing
these modules with an eye towards integrating them in the future. In
principle, the primary difference is that GD::SVG aims to generate SVG
and SVG only.  That is, it:

  1. Does not store an internal representation of the GD image

  2. Does not enable JPG, PNG, OR SVG output from a single pass
     through data

  3. Only occasioanally uses inherited methods from GD

Instead GD::SVG depends on the user to choose which output format they
would like in advance, "use"ing the appropriate module for that
output. As described at the start of this document, module selection
between GD and GD::SVG can be made dynamically using eval statements
and variables for the differnet classes that GD and GD::SVG create.

There is a second reason for not maintaining a double representation
of the data in GD and SVG format: SVG documents can quickly become
very large, especially with large datasets. In cases where scripts are
primarily generating png images in a server environment and would only
occasionally need to export SVG, gernerating an SVG image in parallel
would result in an unacceptable performance hit.

Thus GD::SVG aims to be a plugin for existing configurations that
depend on GD but would like to take advantage of SVG output.

SVG::GD, on the other hand, aims to tie in the raster-editing ability
of GD with the power of SVG output. In part, it aims to do this by
inheriting many methods from GD directly and bringing them into the
functional space of GD.  This makes SVG::GD easier to set up initially
(simply by adding the "use SVG::GD" below the "use GD" statement of
your script. GD::SVG sacrfices this initial ease-of-setup for more
targeted applications.

=head1 ACKNOWLEDGEMENTS

Lincoln Stein, my postdoctoral mentor, author of GD.pm, and all around
Perl stud. Ronan Oger, author of SVG.pm conceptualized and implemented
another wrapper around GD at about the exact same time as this module.
He also provided helpful discussions on implementing GD functions into
SVG.

=head1 AUTHOR

Todd Harris, PhD E<lt>harris@cshl.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Todd Harris and the Cold Spring Harbor Laboratory

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

L<GD>,
L<SVG>,
L<SVG::Manual>,
L<SVG::DOM>

=cut
