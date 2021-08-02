/** Character escaping.
 *
 * The sure files are comrised of ascii characters.  To allow
 * arbitrary characters in filenames (and link targets) we escape
 * characters outside of the printable range, as well as characters
 * that are used to syntactically separate parts of the sure lines. */;

/** Escape the given string. */

let escape: string => string;

/** Reverse the escape operation on the given string. */

let unescape: string => string;
