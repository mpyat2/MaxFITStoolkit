#include "stdafx.h"

extern "C" {
	__declspec(dllexport) const char* __stdcall RawProcessorVersion(void);
	__declspec(dllexport) LibRaw* __stdcall RawProcessorCreate(void);
	__declspec(dllexport) void __stdcall RawProcessorFree(LibRaw* RawProcessor);
	__declspec(dllexport) const char* __stdcall RawProcessorStrError(void* reserved, int p);
	__declspec(dllexport) int __stdcall RawProcessorOpenFile(LibRaw* RawProcessor, char* filename);
	__declspec(dllexport) void __stdcall RawProcessorSizes(
		LibRaw* RawProcessor,
		ushort &raw_width, ushort &raw_height,
		ushort &width, ushort &height,
		ushort &left_margin, ushort &top_margin, 
		ushort &iwidth, ushort &iheight,
		unsigned &raw_pitch,
		double &pixel_aspect,
		int &flip
	);
	__declspec(dllexport) int __stdcall RawProcessorAdjustSizesInfoOnly(LibRaw* RawProcessor);
	__declspec(dllexport) int __stdcall RawProcessorUnpack(LibRaw* RawProcessor);
	__declspec(dllexport) int __stdcall RawProcessorCheck(LibRaw* RawProcessor);
	__declspec(dllexport) ushort* __stdcall RawProcessorRawImage(LibRaw* RawProcessor);
	__declspec(dllexport) time_t __stdcall RawProcessorTimestamp(LibRaw* RawProcessor);
	__declspec(dllexport) void __stdcall RawProcessorTime(LibRaw* RawProcessor, char *timestr, int timest_len);
	__declspec(dllexport) float __stdcall RawProcessorShutter(LibRaw* RawProcessor);
	__declspec(dllexport) float __stdcall RawProcessorISOspeed(LibRaw* RawProcessor);
	__declspec(dllexport) char* __stdcall RawProcessorMake(LibRaw* RawProcessor);
	__declspec(dllexport) char* __stdcall RawProcessorModel(LibRaw* RawProcessor);
	__declspec(dllexport) char* __stdcall RawProcessorSoftware(LibRaw* RawProcessor);
	__declspec(dllexport) void __stdcall RawProcessorBayerPattern(LibRaw* RawProcessor, char *bayer_pattern, int bayer_pattern_len);
}

