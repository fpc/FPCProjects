//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS_CL<p>

   Conversion of OpenCL header file: cl.h to Delphi,
   from http://www.khronos.org/registry/cl/.<p>

   <b>History : </b><font size=-1><ul>
      <li>01/11/09 - DanB - Creation
   </ul></font>
}
// *****************************************************************************
// * Copyright (c) 2008-2009 The Khronos Group Inc.
// *
// * Permission is hereby granted, free of charge, to any person obtaining a
// * copy of this software and/or associated documentation files (the
// * "Materials"), to deal in the Materials without restriction, including
// * without limitation the rights to use, copy, modify, merge, publish,
// * distribute, sublicense, and/or sell copies of the Materials, and to
// * permit persons to whom the Materials are furnished to do so, subject to
// * the following conditions:
// *
// * The above copyright notice and this permission notice shall be included
// * in all copies or substantial portions of the Materials.
// *
// * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
// *****************************************************************************

unit GLS_CL;

interface

uses GLS_CL_Platform;

{$I gls_cl.inc}

const
  LibOpenCL  = 'OpenCL.dll';

type
//typedef struct _cl_platform_id *    cl_platform_id;
//typedef struct _cl_device_id *      cl_device_id;
//typedef struct _cl_context *        cl_context;
//typedef struct _cl_command_queue *  cl_command_queue;
//typedef struct _cl_mem *            cl_mem;
//typedef struct _cl_program *        cl_program;
//typedef struct _cl_kernel *         cl_kernel;
//typedef struct _cl_event *          cl_event;
//typedef struct _cl_sampler *        cl_sampler;

T_cl_platform_id = record end;
T_cl_device_id = record end;
T_cl_context = record end;
T_cl_command_queue = record end;
T_cl_mem = record end;
T_cl_program = record end;
T_cl_kernel = record end;
T_cl_event = record end;
T_cl_sampler = record end;

Tcl_platform_id = ^T_cl_platform_id;
Tcl_device_id = ^T_cl_device_id;
Tcl_context = ^T_cl_context;
Tcl_command_queue = ^T_cl_command_queue;
Tcl_mem = ^T_cl_mem;
Tcl_program = ^T_cl_program;
Tcl_kernel = ^T_cl_kernel;
Tcl_event = ^T_cl_event;
Tcl_sampler = ^T_cl_sampler;

//Tcl_platform_id = NativeUInt;
//Tcl_device_id = NativeUInt;
//Tcl_context = NativeUInt;
//Tcl_command_queue = NativeUInt;
//Tcl_mem = NativeUInt;
//Tcl_program = NativeUInt;     // would this be better?
//Tcl_kernel = NativeUInt;
//Tcl_event = NativeUInt;
//Tcl_sampler = NativeUInt;

Pcl_platform_id = ^Tcl_platform_id;
Pcl_device_id = ^Tcl_device_id;
Pcl_context = ^Tcl_context;
Pcl_command_queue = ^Tcl_command_queue;
Pcl_mem = ^Tcl_mem;
Pcl_program = ^Tcl_program;
Pcl_kernel = ^Tcl_kernel;
Pcl_event = ^Tcl_event;
Pcl_sampler = ^Tcl_sampler;


(* WARNING!  Unlike cl_ types in cl_platform.h, cl_bool is not guaranteed to be
 the same size as the bool in kernels. *)
Tcl_bool = Tcl_uint;
Tcl_bitfield = Tcl_ulong;
Tcl_device_type = Tcl_bitfield;
Tcl_platform_info = Tcl_uint;
Tcl_device_info = Tcl_uint;
Tcl_device_address_info = Tcl_bitfield;
Tcl_device_fp_config = Tcl_bitfield;
Tcl_device_mem_cache_type = Tcl_uint;
Tcl_device_local_mem_type = Tcl_uint;
Tcl_device_exec_capabilities = Tcl_bitfield;
Tcl_command_queue_properties = Tcl_bitfield;

Pcl_bool = ^Tcl_bool;
Pcl_bitfield = ^Tcl_bitfield;
Pcl_device_type = ^Tcl_device_type;
Pcl_platform_info = ^Tcl_platform_info;
Pcl_device_info = ^Tcl_device_info;
Pcl_device_address_info = ^Tcl_device_address_info;
Pcl_device_fp_config = ^Tcl_device_fp_config;
Pcl_device_mem_cache_type = ^Tcl_device_mem_cache_type;
Pcl_device_local_mem_type = ^Tcl_device_local_mem_type;
Pcl_device_exec_capabilities = ^Tcl_device_exec_capabilities;
Pcl_command_queue_properties = ^Tcl_command_queue_properties;


Tcl_context_properties = intptr_t;
Tcl_context_info = Tcl_uint;
Tcl_command_queue_info = Tcl_uint;
Tcl_channel_order = Tcl_uint;
Tcl_channel_type = Tcl_uint;
Tcl_mem_flags = Tcl_bitfield;
Tcl_mem_object_type = Tcl_uint;
Tcl_mem_info = Tcl_uint;
Tcl_image_info = Tcl_uint;
Tcl_addressing_mode = Tcl_uint;
Tcl_filter_mode = Tcl_uint;
Tcl_sampler_info = Tcl_uint;
Tcl_map_flags = Tcl_bitfield;
Tcl_program_info = Tcl_uint;
Tcl_program_build_info = Tcl_uint;
Tcl_build_status = Tcl_int;
Tcl_kernel_info = Tcl_uint;
Tcl_kernel_work_group_info = Tcl_uint;
Tcl_event_info = Tcl_uint;
Tcl_command_type = Tcl_uint;
Tcl_profiling_info = Tcl_uint;

Pcl_context_properties = ^Tcl_context_properties;
Pcl_context_info = ^Tcl_context_info;
Pcl_command_queue_info = ^Tcl_command_queue_info;
Pcl_channel_order = ^Tcl_channel_order;
Pcl_channel_type = ^Tcl_channel_type;
Pcl_mem_flags = ^Tcl_mem_flags;
Pcl_mem_object_type = ^Tcl_mem_object_type;
Pcl_mem_info = ^Tcl_mem_info;
Pcl_image_info = ^Tcl_image_info;
Pcl_addressing_mode = ^Tcl_addressing_mode;
Pcl_filter_mode = ^Tcl_filter_mode;
Pcl_sampler_info = ^Tcl_sampler_info;
Pcl_map_flags = ^Tcl_map_flags;
Pcl_program_info = ^Tcl_program_info;
Pcl_program_build_info = ^Tcl_program_build_info;
Pcl_build_status = ^Tcl_build_status;
Pcl_kernel_info = ^Tcl_kernel_info;
Pcl_kernel_work_group_info = ^Tcl_kernel_work_group_info;
Pcl_event_info = ^Tcl_event_info;
Pcl_command_type = ^Tcl_command_type;
Pcl_profiling_info = ^Tcl_profiling_info;

type
	T_cl_image_format = record
    image_channel_order: Tcl_channel_order;
    image_channel_data_type: Tcl_channel_type;
 end;

 Tcl_image_format	=	^T_cl_image_format;
 Pcl_image_format = ^Tcl_image_format;

// Error Codes
const

CL_SUCCESS =                                  0;
CL_DEVICE_NOT_FOUND =                         -1;
CL_DEVICE_NOT_AVAILABLE =                     -2;
CL_COMPILER_NOT_AVAILABLE =                   -3;
CL_MEM_OBJECT_ALLOCATION_FAILURE =            -4;
CL_OUT_OF_RESOURCES =                         -5;
CL_OUT_OF_HOST_MEMORY =                       -6;
CL_PROFILING_INFO_NOT_AVAILABLE =             -7;
CL_MEM_COPY_OVERLAP =                         -8;
CL_IMAGE_FORMAT_MISMATCH =                    -9;
CL_IMAGE_FORMAT_NOT_SUPPORTED =               -10;
CL_BUILD_PROGRAM_FAILURE =                    -11;
CL_MAP_FAILURE =                              -12;

CL_INVALID_VALUE =                            -30;
CL_INVALID_DEVICE_TYPE =                      -31;
CL_INVALID_PLATFORM =                         -32;
CL_INVALID_DEVICE =                           -33;
CL_INVALID_CONTEXT =                          -34;
CL_INVALID_QUEUE_PROPERTIES =                 -35;
CL_INVALID_COMMAND_QUEUE =                    -36;
CL_INVALID_HOST_PTR =                         -37;
CL_INVALID_MEM_OBJECT =                       -38;
CL_INVALID_IMAGE_FORMAT_DESCRIPTOR =          -39;
CL_INVALID_IMAGE_SIZE =                       -40;
CL_INVALID_SAMPLER =                          -41;
CL_INVALID_BINARY =                           -42;
CL_INVALID_BUILD_OPTIONS =                    -43;
CL_INVALID_PROGRAM =                          -44;
CL_INVALID_PROGRAM_EXECUTABLE =               -45;
CL_INVALID_KERNEL_NAME =                      -46;
CL_INVALID_KERNEL_DEFINITION =                -47;
CL_INVALID_KERNEL =                           -48;
CL_INVALID_ARG_INDEX =                        -49;
CL_INVALID_ARG_VALUE =                        -50;
CL_INVALID_ARG_SIZE =                         -51;
CL_INVALID_KERNEL_ARGS =                      -52;
CL_INVALID_WORK_DIMENSION =                   -53;
CL_INVALID_WORK_GROUP_SIZE =                  -54;
CL_INVALID_WORK_ITEM_SIZE =                   -55;
CL_INVALID_GLOBAL_OFFSET =                    -56;
CL_INVALID_EVENT_WAIT_LIST =                  -57;
CL_INVALID_EVENT =                            -58;
CL_INVALID_OPERATION =                        -59;
CL_INVALID_GL_OBJECT =                        -60;
CL_INVALID_BUFFER_SIZE =                      -61;
CL_INVALID_MIP_LEVEL =                        -62;
CL_INVALID_GLOBAL_WORK_SIZE =                 -63;

// OpenCL Version
CL_VERSION_1_0 =                              1;

// cl_bool
CL_FALSE =                                    0;
CL_TRUE =                                     1;

// cl_platform_info
CL_PLATFORM_PROFILE =                         $0900;
CL_PLATFORM_VERSION =                         $0901;
CL_PLATFORM_NAME =                            $0902;
CL_PLATFORM_VENDOR =                          $0903;
CL_PLATFORM_EXTENSIONS =                      $0904;

// cl_device_type - bitfield
CL_DEVICE_TYPE_DEFAULT =                      (1 shl 0);
CL_DEVICE_TYPE_CPU =                          (1 shl 1);
CL_DEVICE_TYPE_GPU =                          (1 shl 2);
CL_DEVICE_TYPE_ACCELERATOR =                  (1 shl 3);
CL_DEVICE_TYPE_ALL =                          $FFFFFFFF;

// cl_device_info
CL_DEVICE_TYPE =                              $1000;
CL_DEVICE_VENDOR_ID =                         $1001;
CL_DEVICE_MAX_COMPUTE_UNITS =                 $1002;
CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS =          $1003;
CL_DEVICE_MAX_WORK_GROUP_SIZE =               $1004;
CL_DEVICE_MAX_WORK_ITEM_SIZES =               $1005;
CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR =       $1006;
CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT =      $1007;
CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT =        $1008;
CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG =       $1009;
CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT =      $100A;
CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE =     $100B;
CL_DEVICE_MAX_CLOCK_FREQUENCY =               $100C;
CL_DEVICE_ADDRESS_BITS =                      $100D;
CL_DEVICE_MAX_READ_IMAGE_ARGS =               $100E;
CL_DEVICE_MAX_WRITE_IMAGE_ARGS =              $100F;
CL_DEVICE_MAX_MEM_ALLOC_SIZE =                $1010;
CL_DEVICE_IMAGE2D_MAX_WIDTH =                 $1011;
CL_DEVICE_IMAGE2D_MAX_HEIGHT =                $1012;
CL_DEVICE_IMAGE3D_MAX_WIDTH =                 $1013;
CL_DEVICE_IMAGE3D_MAX_HEIGHT =                $1014;
CL_DEVICE_IMAGE3D_MAX_DEPTH =                 $1015;
CL_DEVICE_IMAGE_SUPPORT =                     $1016;
CL_DEVICE_MAX_PARAMETER_SIZE =                $1017;
CL_DEVICE_MAX_SAMPLERS =                      $1018;
CL_DEVICE_MEM_BASE_ADDR_ALIGN =               $1019;
CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE =          $101A;
CL_DEVICE_SINGLE_FP_CONFIG =                  $101B;
CL_DEVICE_GLOBAL_MEM_CACHE_TYPE =             $101C;
CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE =         $101D;
CL_DEVICE_GLOBAL_MEM_CACHE_SIZE =             $101E;
CL_DEVICE_GLOBAL_MEM_SIZE =                   $101F;
CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE =          $1020;
CL_DEVICE_MAX_CONSTANT_ARGS =                 $1021;
CL_DEVICE_LOCAL_MEM_TYPE =                    $1022;
CL_DEVICE_LOCAL_MEM_SIZE =                    $1023;
CL_DEVICE_ERROR_CORRECTION_SUPPORT =          $1024;
CL_DEVICE_PROFILING_TIMER_RESOLUTION =        $1025;
CL_DEVICE_ENDIAN_LITTLE =                     $1026;
CL_DEVICE_AVAILABLE =                         $1027;
CL_DEVICE_COMPILER_AVAILABLE =                $1028;
CL_DEVICE_EXECUTION_CAPABILITIES =            $1029;
CL_DEVICE_QUEUE_PROPERTIES =                  $102A;
CL_DEVICE_NAME =                              $102B;
CL_DEVICE_VENDOR =                            $102C;
CL_DRIVER_VERSION =                           $102D;
CL_DEVICE_PROFILE =                           $102E;
CL_DEVICE_VERSION =                           $102F;
CL_DEVICE_EXTENSIONS =                        $1030;
CL_DEVICE_PLATFORM =                          $1031;

// cl_device_fp_config - bitfield
CL_FP_DENORM =                                (1 shl 0);
CL_FP_INF_NAN =                               (1 shl 1);
CL_FP_ROUND_TO_NEAREST =                      (1 shl 2);
CL_FP_ROUND_TO_ZERO =                         (1 shl 3);
CL_FP_ROUND_TO_INF =                          (1 shl 4);
CL_FP_FMA =                                   (1 shl 5);

// cl_device_mem_cache_type
CL_NONE =                                     $0;
CL_READ_ONLY_CACHE =                          $1;
CL_READ_WRITE_CACHE =                         $2;

// cl_device_local_mem_type
CL_LOCAL =                                    $1;
CL_GLOBAL =                                   $2;

// cl_device_exec_capabilities - bitfield
CL_EXEC_KERNEL =                              (1 shl 0);
CL_EXEC_NATIVE_KERNEL =                       (1 shl 1);

// cl_command_queue_properties - bitfield
CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE =      (1 shl 0);
CL_QUEUE_PROFILING_ENABLE =                   (1 shl 1);

// cl_context_info
CL_CONTEXT_REFERENCE_COUNT =                  $1080;
CL_CONTEXT_DEVICES =                          $1081;
CL_CONTEXT_PROPERTIES =                       $1082;

// cl_context_properties
CL_CONTEXT_PLATFORM =                         $1084;

// cl_command_queue_info
CL_QUEUE_CONTEXT =                            $1090;
CL_QUEUE_DEVICE =                             $1091;
CL_QUEUE_REFERENCE_COUNT =                    $1092;
CL_QUEUE_PROPERTIES =                         $1093;

// cl_mem_flags - bitfield
CL_MEM_READ_WRITE =                           (1 shl 0);
CL_MEM_WRITE_ONLY =                           (1 shl 1);
CL_MEM_READ_ONLY =                            (1 shl 2);
CL_MEM_USE_HOST_PTR =                         (1 shl 3);
CL_MEM_ALLOC_HOST_PTR =                       (1 shl 4);
CL_MEM_COPY_HOST_PTR =                        (1 shl 5);

// cl_channel_order
CL_R =                                        $10B0;
CL_A =                                        $10B1;
CL_RG =                                       $10B2;
CL_RA =                                       $10B3;
CL_RGB =                                      $10B4;
CL_RGBA =                                     $10B5;
CL_BGRA =                                     $10B6;
CL_ARGB =                                     $10B7;
CL_INTENSITY =                                $10B8;
CL_LUMINANCE =                                $10B9;

// cl_channel_type
CL_SNORM_INT8 =                               $10D0;
CL_SNORM_INT16 =                              $10D1;
CL_UNORM_INT8 =                               $10D2;
CL_UNORM_INT16 =                              $10D3;
CL_UNORM_SHORT_565 =                          $10D4;
CL_UNORM_SHORT_555 =                          $10D5;
CL_UNORM_INT_101010 =                         $10D6;
CL_SIGNED_INT8 =                              $10D7;
CL_SIGNED_INT16 =                             $10D8;
CL_SIGNED_INT32 =                             $10D9;
CL_UNSIGNED_INT8 =                            $10DA;
CL_UNSIGNED_INT16 =                           $10DB;
CL_UNSIGNED_INT32 =                           $10DC;
CL_HALF_FLOAT =                               $10DD;
CL_FLOAT =                                    $10DE;

// cl_mem_object_type
CL_MEM_OBJECT_BUFFER =                        $10F0;
CL_MEM_OBJECT_IMAGE2D =                       $10F1;
CL_MEM_OBJECT_IMAGE3D =                       $10F2;

// cl_mem_info
CL_MEM_TYPE =                                 $1100;
CL_MEM_FLAGS =                                $1101;
CL_MEM_SIZE =                                 $1102;
CL_MEM_HOST_PTR =                             $1103;
CL_MEM_MAP_COUNT =                            $1104;
CL_MEM_REFERENCE_COUNT =                      $1105;
CL_MEM_CONTEXT =                              $1106;

// cl_image_info
CL_IMAGE_FORMAT =                             $1110;
CL_IMAGE_ELEMENT_SIZE =                       $1111;
CL_IMAGE_ROW_PITCH =                          $1112;
CL_IMAGE_SLICE_PITCH =                        $1113;
CL_IMAGE_WIDTH =                              $1114;
CL_IMAGE_HEIGHT =                             $1115;
CL_IMAGE_DEPTH =                              $1116;

// cl_addressing_mode
CL_ADDRESS_NONE =                             $1130;
CL_ADDRESS_CLAMP_TO_EDGE =                    $1131;
CL_ADDRESS_CLAMP =                            $1132;
CL_ADDRESS_REPEAT =                           $1133;

// cl_filter_mode
CL_FILTER_NEAREST =                           $1140;
CL_FILTER_LINEAR =                            $1141;

// cl_sampler_info
CL_SAMPLER_REFERENCE_COUNT =                  $1150;
CL_SAMPLER_CONTEXT =                          $1151;
CL_SAMPLER_NORMALIZED_COORDS =                $1152;
CL_SAMPLER_ADDRESSING_MODE =                  $1153;
CL_SAMPLER_FILTER_MODE =                      $1154;

// cl_map_flags - bitfield
CL_MAP_READ =                                 (1 shl 0);
CL_MAP_WRITE =                                (1 shl 1);

// cl_program_info
CL_PROGRAM_REFERENCE_COUNT =                  $1160;
CL_PROGRAM_CONTEXT =                          $1161;
CL_PROGRAM_NUM_DEVICES =                      $1162;
CL_PROGRAM_DEVICES =                          $1163;
CL_PROGRAM_SOURCE =                           $1164;
CL_PROGRAM_BINARY_SIZES =                     $1165;
CL_PROGRAM_BINARIES =                         $1166;

// cl_program_build_info
CL_PROGRAM_BUILD_STATUS =                     $1181;
CL_PROGRAM_BUILD_OPTIONS =                    $1182;
CL_PROGRAM_BUILD_LOG =                        $1183;

// cl_build_status
CL_BUILD_SUCCESS =                            0;
CL_BUILD_NONE =                               -1;
CL_BUILD_ERROR =                              -2;
CL_BUILD_IN_PROGRESS =                        -3;

// cl_kernel_info
CL_KERNEL_FUNCTION_NAME =                     $1190;
CL_KERNEL_NUM_ARGS =                          $1191;
CL_KERNEL_REFERENCE_COUNT =                   $1192;
CL_KERNEL_CONTEXT =                           $1193;
CL_KERNEL_PROGRAM =                           $1194;

// cl_kernel_work_group_info
CL_KERNEL_WORK_GROUP_SIZE =                   $11B0;
CL_KERNEL_COMPILE_WORK_GROUP_SIZE =           $11B1;
CL_KERNEL_LOCAL_MEM_SIZE =                    $11B2;

// cl_event_info
CL_EVENT_COMMAND_QUEUE =                      $11D0;
CL_EVENT_COMMAND_TYPE =                       $11D1;
CL_EVENT_REFERENCE_COUNT =                    $11D2;
CL_EVENT_COMMAND_EXECUTION_STATUS =           $11D3;

// cl_command_type
CL_COMMAND_NDRANGE_KERNEL =                   $11F0;
CL_COMMAND_TASK =                             $11F1;
CL_COMMAND_NATIVE_KERNEL =                    $11F2;
CL_COMMAND_READ_BUFFER =                      $11F3;
CL_COMMAND_WRITE_BUFFER =                     $11F4;
CL_COMMAND_COPY_BUFFER =                      $11F5;
CL_COMMAND_READ_IMAGE =                       $11F6;
CL_COMMAND_WRITE_IMAGE =                      $11F7;
CL_COMMAND_COPY_IMAGE =                       $11F8;
CL_COMMAND_COPY_IMAGE_TO_BUFFER =             $11F9;
CL_COMMAND_COPY_BUFFER_TO_IMAGE =             $11FA;
CL_COMMAND_MAP_BUFFER =                       $11FB;
CL_COMMAND_MAP_IMAGE =                        $11FC;
CL_COMMAND_UNMAP_MEM_OBJECT =                 $11FD;
CL_COMMAND_MARKER =                           $11FE;
CL_COMMAND_ACQUIRE_GL_OBJECTS =               $11FF;
CL_COMMAND_RELEASE_GL_OBJECTS =               $1200;

// command execution status
CL_COMPLETE =                                 $0;
CL_RUNNING =                                  $1;
CL_SUBMITTED =                                $2;
CL_QUEUED =                                   $3;

// cl_profiling_info
CL_PROFILING_COMMAND_QUEUED =                 $1280;
CL_PROFILING_COMMAND_SUBMIT =                 $1281;
CL_PROFILING_COMMAND_START =                  $1282;
CL_PROFILING_COMMAND_END =                    $1283;

//*********************************************************************

// Platform API
function clGetPlatformIDs(num_entries: Tcl_uint;
                          platforms: Pcl_platform_id;
                          num_platforms: Pcl_uint): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                             {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                             external LibOpenCL;

function clGetPlatformInfo(_platform: Tcl_platform_id;
                           param_name: Tcl_platform_info;
                           param_value_size: size_t;
                           param_value: Pointer;
                           param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                    {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                    external LibOpenCL;

// Device APIs
function clGetDeviceIDs(_platform: Tcl_platform_id;
                        device_type: Tcl_device_type;
                        num_entries: Tcl_uint;
                        devices: Pcl_device_id;
                        num_devices: Pcl_uint): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                         external LibOpenCL;

function clGetDeviceInfo(device: Tcl_device_id;
                         param_name: Tcl_device_info;
                         param_value_size: size_t;
                         param_value: Pointer;
                         param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                  {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                  external LibOpenCL;

// Context APIs

type logging_fn = procedure(errinfo: PAnsiChar; private_info: Pointer; cb: size_t; user_data: Pointer); {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                                                        {$IFDEF CL_CDECL} cdecl;{$ENDIF}
//typedef void (*logging_fn)(const char *, const void *, size_t, const void *);

function clCreateContext(properties: Pcl_context_properties;
                         num_devices: Tcl_uint;
                         devices: Pcl_device_id;
                         pfn_notify: logging_fn;
                         user_data: Pointer;
                         errcode_ret: Pcl_int): Tcl_context; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                             {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                             external LibOpenCL;

function clCreateContextFromType(properties: Pcl_context_properties;
                                 device_type: Tcl_device_type;
                                 pfn_notify: logging_fn;
                                 user_data: Pointer;
                                 errcode_ret: Pcl_int): Tcl_context; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                     {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                     external LibOpenCL;

function clRetainContext(context: Tcl_context): Tcl_context; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                             {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                             external LibOpenCL;

function clReleaseContext(context: Tcl_context): Tcl_context; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                              {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                              external LibOpenCL;

function clGetContextInfo(context: Tcl_context;
                          param_name: Tcl_context_info;
                          param_value_size: size_t;
                          param_value: Pointer;
                          param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                   {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                   external LibOpenCL;

// Command Queue APIs

function clCreateCommandQueue(context: Tcl_context;
                              device: Tcl_device_id;
                              properties: Tcl_command_queue_properties;
                              errcode_ret: Pcl_int): Tcl_command_queue; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                        {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                        external LibOpenCL;

function clRetainCommandQueue(command_queue: Tcl_command_queue): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                          {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                          external LibOpenCL;

function clReleaseCommandQueue(command_queue: Tcl_command_queue): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                           external LibOpenCL;

function clGetCommandQueueInfo(command_queue: Tcl_command_queue;
                               param_name: Tcl_command_queue_info;
                               param_value_size: size_t;
                               param_value: Pointer;
                               param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                        {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                        external LibOpenCL;

function clSetCommandQueueProperty(command_queue: Tcl_command_queue;
                                   properties: Tcl_command_queue_properties;
                                   enable: Tcl_bool;
                                   old_properties: Pcl_command_queue_properties): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                                           external LibOpenCL;

// Memory Object APIs

function clCreateBuffer(context: Tcl_context;
                        flags: Tcl_mem_flags;
                        size: size_t;
                        host_ptr: Pointer;
                        errcode_ret: Pcl_int): Tcl_mem; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                        {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                        external LibOpenCL;

function clCreateImage2D(context: Tcl_context;
                         flags: Tcl_mem_flags;
                         image_format: Pcl_image_format;
                         image_width: size_t;
                         image_height: size_t;
                         image_row_pitch: size_t;
                         host_ptr: Pointer;
                         errcode_ret: Pcl_int): Tcl_mem; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                         external LibOpenCL;

function clCreateImage3D(context: Tcl_context;
                         flags: Tcl_mem_flags;
                         image_format: Pcl_image_format;
                         image_width: size_t;
                         image_height: size_t;
                         image_depth: size_t;
                         image_row_pitch: size_t;
                         image_slice_pitch: size_t;
                         host_ptr: Pointer;
                         errcode_ret: Pcl_int): Tcl_mem; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                         external LibOpenCL;

function clRetainMemObject(memobj: Tcl_mem): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                      {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                      external LibOpenCL;

function clReleaseMemObject(memobj: Tcl_mem): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                       {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                       external LibOpenCL;

function clGetSupportedImageFormats(context: Tcl_context;
                                    flags: Tcl_mem_flags;
                                    image_type: Tcl_mem_object_type;
                                    num_entries: Tcl_uint;
                                    image_formats: Pcl_image_format;
                                    num_image_formats: Pcl_uint): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                           external LibOpenCL;

function clGetMemObjectInfo(memobj: Tcl_mem;
                            param_name: Tcl_mem_info;
                            param_value_size: size_t;
                            param_value: Pointer;
                            param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                     {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                     external LibOpenCL;

function clGetImageInfo(image: Tcl_mem;
                        param_name: Tcl_image_info;
                        param_value_size: size_t;
                        param_value: Pointer;
                        param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                 {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                 external LibOpenCL;

// Sampler APIs

function clCreateSampler(context: Tcl_context;
                         normalized_coords: Tcl_bool;
                         addressing_mode: Tcl_addressing_mode;
                         filter_mode: Tcl_filter_mode;
                         errcode_ret: Pcl_int): Tcl_sampler; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                             {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                             external LibOpenCL;

function clRetainSampler(sampler: Tcl_sampler): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                         external LibOpenCL;

function clReleaseSampler(sampler: Tcl_sampler): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                          {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                          external LibOpenCL;

function clGetSamplerInfo(sampler: Tcl_sampler;
                          param_name: Tcl_sampler_info;
                          param_value_size: size_t;
                          param_value: Pointer;
                          param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                   {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                   external LibOpenCL;

// Program Object APIs

function clCreateProgramWithSource(context: Tcl_context;
                                   count: Tcl_uint;
                                   strings: PPAnsiChar;
                                   lengths: Psize_t;
                                   errcode_ret: Pcl_int): Tcl_program; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                       {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                       external LibOpenCL;

function clCreateProgramWithBinary(context: Tcl_context;
                                   num_devices: Tcl_uint;
                                   device_list: Pcl_device_id;
                                   lengths: Psize_t;
                                   binaries: PPointer; // TODO: change to PPByte?
                                   binary_status: Pcl_int;
                                   errcode_ret: Pcl_int): Tcl_program; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                       {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                       external LibOpenCL;

//extern CL_API_ENTRY cl_program CL_API_CALL
//clCreateProgramWithBinary(cl_context            /* context */,
//                          cl_uint               /* num_devices */,
//                          const cl_device_id *  /* device_list */,
//                          const size_t *        /* lengths */,
//                          const char **         /* binaries */,
//                          cl_int *              /* binary_status */,
//                          cl_int *              /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;
//

function clRetainProgram(_program: Tcl_program): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                          {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                          external LibOpenCL;

function clReleaseProgram(_program: Tcl_program): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                           external LibOpenCL;

type ProgramBuilt_notify_fn = procedure(_program: Tcl_program; user_data: Pointer); {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                                    {$IFDEF CL_CDECL} cdecl;{$ENDIF}

function clBuildProgram(_program: Tcl_program;
                        num_devices: Tcl_uint;
                        device_list: Pcl_device_id;
                        options: PAnsiChar;
                        pfn_notify: ProgramBuilt_notify_fn;
                        user_data: Pointer): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                      {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                      external LibOpenCL;

function clUnloadCompiler(): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                      {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                      external LibOpenCL;

function clGetProgramInfo(_program: Tcl_program;
                          param_name: Tcl_program_info;
                          param_value_size: size_t;
                          param_value: Pointer;
                          param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                   {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                   external LibOpenCL;

function clGetProgramBuildInfo(_program: Tcl_program;
                               device: Tcl_device_id;
                               param_name: Tcl_program_build_info;
                               param_value_size: size_t;
                               param_value: Pointer;
                               param_value_size_ret: Psize_t): Tcl_int;  {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                         external LibOpenCL;

// Kernel Object APIs

function clCreateKernel(_program: Tcl_program;
                        kernel_name: PAnsiChar;
                        errcode_ret: Pcl_int): Tcl_kernel; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                           external LibOpenCL;

function clCreateKernelsInProgram(_program: Tcl_program;
                                  num_kernels: Tcl_uint;
                                  kernels: Pcl_kernel;
                                  num_kernels_ret: Pcl_uint): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                       {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                       external LibOpenCL;

function clRetainKernel(kernel: Tcl_kernel): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                      {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                      external LibOpenCL;

function clReleaseKernel(kernel: Tcl_kernel): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                       {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                       external LibOpenCL;

function clSetKernelArg(kernel: Tcl_kernel;
                        arg_index: Tcl_uint;
                        arg_size: size_t;
                        arg_value: Pointer): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                      {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                      external LibOpenCL;

function clGetKernelInfo(kernel: Tcl_kernel;
                         param_name: Tcl_kernel_info;
                         param_value_size: size_t;
                         param_value: Pointer;
                         param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                  {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                  external LibOpenCL;

function clGetKernelWorkGroupInfo(kernel: Tcl_kernel;
                                  device: Tcl_device_id;
                                  param_name: Tcl_kernel_work_group_info;
                                  param_value_size: size_t;
                                  param_value: Pointer;
                                  param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                           external LibOpenCL;

// Event Object APIs

function clWaitForEvents(num_events: Tcl_uint;
                         event_list: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                          {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                          external LibOpenCL;

function clGetEventInfo(event: Tcl_event;
                        param_name: Tcl_event_info;
                        param_value_size: size_t;
                        param_value: Pointer;
                        param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                 {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                 external LibOpenCL;

function clRetainEvent(event: Tcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                   {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                   external LibOpenCL;

function clReleaseEvent(event: Tcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                    {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                    external LibOpenCL;

// Profiling APIs

function clGetEventProfilingInfo(event: Tcl_event;
                                 param_name: Tcl_profiling_info;
                                 param_value_size: size_t;
                                 param_value: Pointer;
                                 param_value_size_ret: Psize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                          {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                          external LibOpenCL;

// Flush and Finish APIs

function clFlush(command_queue: Tcl_command_queue): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                             {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                             external LibOpenCL;

function clFinish(command_queue: Tcl_command_queue): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                              {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                              external LibOpenCL;

// Enqueued Commands APIs

function clEnqueueReadBuffer(command_queue: Tcl_command_queue;
                             buffer: Tcl_mem;
                             blocking_read: Tcl_bool;
                             offset: size_t;
                             cb: size_t;
                             ptr: Pointer;
                             num_events_in_wait_list: Tcl_uint;
                             event_wait_list: Pcl_event;
                             event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                         external LibOpenCL;

function clEnqueueWriteBuffer(command_queue: Tcl_command_queue;
                              buffer: Tcl_mem;
                              blocking_write: Tcl_bool;
                              offset: size_t;
                              cb: size_t;
                              ptr: Pointer;
                              num_events_in_wait_list: Tcl_uint;
                              event_wait_list: Pcl_event;
                              event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                          {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                          external LibOpenCL;

function clEnqueueCopyBuffer(command_queue: Tcl_command_queue;
                             src_buffer: Tcl_mem;
                             dst_buffer: Tcl_mem;
                             src_offset: size_t;
                             dst_offset: size_t;
                             cb: size_t;
                             num_events_in_wait_list: Tcl_uint;
                             event_wait_list: Pcl_event;
                             event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                         external LibOpenCL;

function clEnqueueReadImage(command_queue: Tcl_command_queue;
                            image: Tcl_mem;
                            blocking_read: Tcl_bool;
                            origin: Psize_t;// x3
                            region: Psize_t;// x3
                            row_pitch: size_t;
                            slice_pitch: size_t;
                            ptr: Pointer;
                            num_events_in_wait_list: Tcl_uint;
                            event_wait_list: Pcl_event;
                            event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                        {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                        external LibOpenCL;

function clEnqueueWriteImage(command_queue: Tcl_command_queue;
                             image: Tcl_mem;
                             blocking_write: Tcl_bool;
                             origin: Psize_t;// x3
                             region: Psize_t;// x3
                             input_row_pitch: size_t;
                             input_slice_pitch: size_t;
                             ptr: Pointer;
                             num_events_in_wait_list: Tcl_uint;
                             event_wait_list: Pcl_event;
                             event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                         {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                         external LibOpenCL;

function clEnqueueCopyImage(command_queue: Tcl_command_queue;
                            src_image: Tcl_mem;
                            dst_image: Tcl_mem;
                            src_origin: Psize_t;//x3
                            dst_origin: Psize_t;//x3
                            region: PSize_t;//x3
                            num_events_in_wait_list: Tcl_uint;
                            event_wait_list: Pcl_event;
                            event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                        {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                        external LibOpenCL;

function clEnqueueCopyImageToBuffer(command_queue: Tcl_command_queue;
                                    src_image: Tcl_mem;
                                    dst_buffer: Tcl_mem;
                                    src_origin: Psize_t;//x3
                                    region: Psize_t;//x3
                                    dst_offset: size_t;
                                    num_events_in_wait_list: Tcl_uint;
                                    event_wait_list: Pcl_event;
                                    event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                external LibOpenCL;

function clEnqueueCopyBufferToImage(command_queue: Tcl_command_queue;
                                    src_buffer: Tcl_mem;
                                    dst_image: Tcl_mem;
                                    src_offset: size_t;
                                    dst_origin: Psize_t;//x3
                                    region: Psize_t;//x3
                                    num_events_in_wait_list: Tcl_uint;
                                    event_wait_list: Pcl_event;
                                    event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                external LibOpenCL;

function clEnqueueMapBuffer(command_queue: Tcl_command_queue;
                            buffer: Tcl_mem;
                            blocking_map: Tcl_bool;
                            map_flags: Tcl_map_flags;
                            offset: size_t;
                            cb: size_t;
                            num_events_in_wait_list: Tcl_uint;
                            event_wait_list: Pcl_event;
                            event: Pcl_event;
                            errcode_ret: Pcl_int): Pointer; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                            {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                            external LibOpenCL;

function clEnqueueMapImage(command_queue: Tcl_command_queue;
                           image: Tcl_mem;
                           blocking_map: Tcl_bool;
                           map_flags: Tcl_map_flags;
                           origin: Psize_t;//x3
                           region: Psize_t;//x3
                           image_row_pitch: Psize_t;
                           image_slice_pitch: Psize_t;
                           num_events_in_wait_list: Tcl_uint;
                           event_wait_list: Pcl_event;
                           event: Pcl_event;
                           errcode_ret: Pcl_int): Pointer; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                           external LibOpenCL;

function clEnqueueUnmapMemObject(command_queue: Tcl_command_queue;
                                 memobj: Tcl_mem;
                                 mapped_ptr: Pointer;
                                 num_events_in_wait_list: Tcl_uint;
                                 event_wait_list: Pcl_event;
                                 event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                             {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                             external LibOpenCL;

function clEnqueueNDRangeKernel(command_queue: Tcl_command_queue;
                                kernel: Tcl_kernel;
                                work_dim: Tcl_uint;
                                global_work_offset: Psize_t;
                                global_work_size: Psize_t;
                                local_work_size: Psize_t;
                                num_events_in_wait_list: Tcl_uint;
                                event_wait_list: Pcl_event;
                                event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                            {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                            external LibOpenCL;

function clEnqueueTask(command_queue: Tcl_command_queue;
                       kernel: Tcl_kernel;
                       num_events_in_wait_list: Tcl_uint;
                       event_wait_list: Pcl_event;
                       event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                   {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                   external LibOpenCL;

type EnqueueNativeKernel_user_func = procedure(); {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                  {$IFDEF CL_CDECL} cdecl;{$ENDIF}

function clEnqueueNativeKernel(command_queue: Tcl_command_queue;
                               user_func: EnqueueNativeKernel_user_func;
                               args: Pointer;
                               cb_args: size_t;
                               num_mem_objects: Tcl_uint;
                               mem_list: Pcl_mem;
                               args_mem_loc: PPointer;
                               num_events_in_wait_list: Tcl_uint;
                               event_wait_list: Pcl_event;
                               event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                           {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                           external LibOpenCL;

function clEnqueueMarker(command_queue: Tcl_command_queue;
                         event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                     {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                     external LibOpenCL;

function clEnqueueWaitForEvents(command_queue: Tcl_command_queue;
                                num_events: Tcl_uint;
                                event_list: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                 {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                 external LibOpenCL;

function clEnqueueBarrier(command_queue: Tcl_command_queue): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                      {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                      external LibOpenCL;

// Extension function access
//
// Returns the extension function address for the given function name,
// or NULL if a valid function can not be found.  The client must
// check to make sure the address is not NULL, before using or
// calling the returned function address.
//

function clGetExtensionFunctionAddress(func_name: PAnsiChar): Pointer; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                       {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                       external LibOpenCL;

implementation

end.
