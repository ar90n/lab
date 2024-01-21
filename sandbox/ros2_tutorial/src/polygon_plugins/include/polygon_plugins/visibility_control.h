#ifndef POLYGON_PLUGINS__VISIBILITY_CONTROL_H_
#define POLYGON_PLUGINS__VISIBILITY_CONTROL_H_

// This logic was borrowed (then namespaced) from the examples on the gcc wiki:
//     https://gcc.gnu.org/wiki/Visibility

#if defined _WIN32 || defined __CYGWIN__
  #ifdef __GNUC__
    #define POLYGON_PLUGINS_EXPORT __attribute__ ((dllexport))
    #define POLYGON_PLUGINS_IMPORT __attribute__ ((dllimport))
  #else
    #define POLYGON_PLUGINS_EXPORT __declspec(dllexport)
    #define POLYGON_PLUGINS_IMPORT __declspec(dllimport)
  #endif
  #ifdef POLYGON_PLUGINS_BUILDING_LIBRARY
    #define POLYGON_PLUGINS_PUBLIC POLYGON_PLUGINS_EXPORT
  #else
    #define POLYGON_PLUGINS_PUBLIC POLYGON_PLUGINS_IMPORT
  #endif
  #define POLYGON_PLUGINS_PUBLIC_TYPE POLYGON_PLUGINS_PUBLIC
  #define POLYGON_PLUGINS_LOCAL
#else
  #define POLYGON_PLUGINS_EXPORT __attribute__ ((visibility("default")))
  #define POLYGON_PLUGINS_IMPORT
  #if __GNUC__ >= 4
    #define POLYGON_PLUGINS_PUBLIC __attribute__ ((visibility("default")))
    #define POLYGON_PLUGINS_LOCAL  __attribute__ ((visibility("hidden")))
  #else
    #define POLYGON_PLUGINS_PUBLIC
    #define POLYGON_PLUGINS_LOCAL
  #endif
  #define POLYGON_PLUGINS_PUBLIC_TYPE
#endif

#endif  // POLYGON_PLUGINS__VISIBILITY_CONTROL_H_
