// LibRawMxWrapper.cpp
//

#include "stdafx.h"

#define S RawProcessor->imgdata.sizes
#define I RawProcessor->imgdata.idata
#define R RawProcessor->imgdata.rawdata
#define O RawProcessor->imgdata.other

__declspec(dllexport) const char* __stdcall RawProcessorVersion(void)
{
	return LibRaw::version();
}

__declspec(dllexport) LibRaw* __stdcall RawProcessorCreate(void)
{
	return new LibRaw;
}

__declspec(dllexport) void __stdcall RawProcessorFree(LibRaw* RawProcessor)
{
	delete RawProcessor;
}

__declspec(dllexport) const char* __stdcall RawProcessorStrError(void* reserved, int p)
{
	return LibRaw::strerror(p);
}

__declspec(dllexport) int __stdcall RawProcessorOpenFile(LibRaw* RawProcessor, char* filename)
{
	return RawProcessor->open_file(filename);
}

__declspec(dllexport) void __stdcall RawProcessorSizes(
	LibRaw* RawProcessor,
	ushort &raw_width, ushort &raw_height,
	ushort &width, ushort &height,
	ushort &left_margin, ushort &top_margin, 
	ushort &iwidth, ushort &iheight,
	unsigned &raw_pitch,
	double &pixel_aspect,
	int &flip
)
{
	raw_width = S.raw_width;
	raw_height = S.raw_height;
	width = S.width;
	height = S.height;
	left_margin = S.left_margin;
	top_margin = S.top_margin;
	iwidth = S.iwidth;
	iheight = S.iheight;
	raw_pitch = S.raw_pitch;
	pixel_aspect = S.pixel_aspect;
	flip = S.flip;
}

__declspec(dllexport) int __stdcall RawProcessorAdjustSizesInfoOnly(LibRaw* RawProcessor)
{
	return RawProcessor->adjust_sizes_info_only();
}

__declspec(dllexport) int __stdcall RawProcessorUnpack(LibRaw* RawProcessor)
{
	return RawProcessor->unpack();
}

__declspec(dllexport) int __stdcall RawProcessorCheck(LibRaw* RawProcessor)
{
	if (!(I.filters || I.colors == 1)) return -1; else return 0;
}

__declspec(dllexport) ushort* __stdcall RawProcessorRawImage(LibRaw* RawProcessor)
{

	return R.raw_image;
}

__declspec(dllexport) time_t __stdcall RawProcessorTimestamp(LibRaw* RawProcessor)
{
	return O.timestamp;
}

__declspec(dllexport) void __stdcall RawProcessorTime(LibRaw* RawProcessor, char *timestr, int timest_len)
{
	time_t t = O.timestamp;
	ctime_s(timestr, timest_len, &t);
}

__declspec(dllexport) float __stdcall RawProcessorShutter(LibRaw* RawProcessor)
{
	return O.shutter;
}

__declspec(dllexport) float __stdcall RawProcessorISOspeed(LibRaw* RawProcessor)
{
	return O.iso_speed;
}

__declspec(dllexport) char* __stdcall RawProcessorMake(LibRaw* RawProcessor)
{
	return R.iparams.make;
}

__declspec(dllexport) char* __stdcall RawProcessorModel(LibRaw* RawProcessor)
{
	return R.iparams.model;
}

__declspec(dllexport) char* __stdcall RawProcessorSoftware(LibRaw* RawProcessor)
{
	return R.iparams.software;
}

__declspec(dllexport) void __stdcall RawProcessorBayerPattern(LibRaw* RawProcessor, char *bayer_pattern, int bayer_pattern_len)
{
	char color_desc[5];
	for (int i = 0; i < 5; i++) color_desc[i] = I.cdesc[i];
	//putchar('\n'); for (int i = 0; i < 5; i++) { putchar('"'); putchar(color_desc[i]); putchar('"'); putchar('\n'); }
	if (color_desc[3]) color_desc[3] = 'G';
    for (int i = 0; i < 16 || i < bayer_pattern_len - 1; i++)
	{
		bayer_pattern[i] = color_desc[RawProcessor->fcol(i >> 1, i & 1)];
		bayer_pattern[i+1] = '\0';
	}

}

#undef O
#undef R
#undef I
#undef S
