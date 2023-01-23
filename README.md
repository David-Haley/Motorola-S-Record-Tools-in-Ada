# Motorola S Record Tools

DisplayHexM displays the contents of an S record file in debug format, including the representation of all printable characters.

CompHexM compares two S record files. The files are considered the same if the results of programming two EPROMS (e.g. 27xx 27xxx devices which default to FF when un-programed) would result in the EPROMs being identical. Comparison is independent of the record length.

the Motorol_Prom package supports reading and writing S record files up to the full 24 bit (6 byte address) range. Including the S0 header and specifying an entry address via an S8 or S9 record. 


