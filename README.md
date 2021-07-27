
# Source code for Multics Release 12.6f

[Multics](https://multicians.org/) is a
[pioneering](https://multicians.org/history.html) operating system,
commonly considered the "progenitor" of the UNIX operating system.

This directory tree [archive](multics-source.mr12.6f.cpio.gz) contains
most of the source code for Multics Release 12.6f.  MR12.6f is the
most recent release of Multics(*).  It is based on the [source code
for the original
Multics](http://web.mit.edu/multics-history/source/Multics_Internet_Server/Multics_sources.html),
which was generously donated to MIT by Bull HN, and which was
subsequently posted on the web.

(*) Actually, as of the time of the creation of this source code
archive, MR12.7 was in the final stages of release preparation.

A dedicated group of Multicians used that source code to reconstruct a
[version of Multics](https://multics-wiki.swenson.org) that runs on
the [dps8m simulator](https://multicians.org/simulator.html).  There
have been several subsequent releases to fix various minor bugs and to
add features that simplify interaction with currently popular systems.

The copyright status of this source code is complicated.  See the 
[COPYRIGHT.md](COPYRIGHT.md) or [COPYRIGHT.html](COPYRIGHT.html) files.

The enhanced version of Multics is self-hosting using the same
filesystem hierarchy structure used in original Multics.  The source
code herein has been copied from an operating instance of MR12.6f.  It
has had to be pieced together due to some quirks of the adaptation of
tapes.  (In the original Multics, dump tapes would have been mounted
and ummounted by human operators.  dps8m does not simulate those
humans.)

## Archive Contents

The Multics source code herein was obtained as follows:

* A Multics system was created using normal installation and upgrade
  procedures.

* The >ldd, >info, and >sl3p hierarchies were dumped (using the normal
  backup_dump tool) to (simulated) tape.  This was done as multiple
  subsets, so some files might have been accidentally skipped.

* The contents of those tapes was extracted using
  [mxload](https://multicians.org/mxload.html).  This tool performs
  numerous transformations to adapt Multics filesystem conventions
  to UNIX filesystem conventions.  The most notable are that:  1) it
  converts '>' to '/' in pathnames; and 2) it converts Multics
  ".archive" files into a directory containing the components as
  separate files.

* The "object" directories have been excluded.  These directories
  contain a form of Multics object files.  They are large and there
  are few tools on non-Multics systems to deal with them.  (If you
  want to analyze these files, you probably need to do that on an
  actual running Multics system.)  This general exclusion does also
  exclude ".bind" files, which are similar to link editor files on
  other systems, but these are rarely useful in understanding the
  Multics system.

* The resulting hierarchy was archived using cpio and compressed using
  gzip.  (Note that it is cpio that is used, not tar -- because I
  prefer the cpio format and have more tools available to me that
  manipulate cpio files.)

## Bug Reports and Discussion

Any problems should be submitted to:
        dmw DOT technical AT gmail DOT com

(210726 - dmw)

