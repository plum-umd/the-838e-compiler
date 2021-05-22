#include <SDL2/SDL.h>
#include "villain.h"

struct Pixel {
  Uint8 b; Uint8 g; Uint8 r; Uint8 a;
};

static SDL_Window *win = NULL;
static SDL_Renderer *renderer = NULL;
static SDL_Texture *texture = NULL;

/* args:
 *  - width: int
 *  - height: int
 *  - scale: int
 * returns:
 *  - void
 */
vl_val sdl_init(uint64_t argc, vl_val *argv)
{
  int64_t width, height, scale;

  vl_check_arity(argc, 3);
  vl_check_type(argv[0], VL_INT);
  vl_check_type(argv[1], VL_INT);
  vl_check_type(argv[2], VL_INT);

  width = vl_unwrap_int(argv[0]);
  height = vl_unwrap_int(argv[1]);
  scale = vl_unwrap_int(argv[2]);

  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindowAndRenderer(width*scale, height*scale, 0,
                              &win, &renderer);
  texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_BGRA32,
                              SDL_TEXTUREACCESS_STREAMING,
                              width, height);
  return vl_wrap_void();
}

/* args: none
 * returns:
 *  - void
 */
vl_val sdl_poll_events(uint64_t argc, vl_val *argv)
{
  static Uint32 ntick = 0;
  static Uint32 framedelay = 1000 / 60;
  SDL_Event event;

  /* cap fps */
  Uint32 tick = SDL_GetTicks();
  if (tick < ntick)
    SDL_Delay(ntick - tick);
  ntick = SDL_GetTicks() + framedelay;

  while (SDL_PollEvent(&event)) {
    if (event.type == SDL_QUIT)
      exit(0);
  }
  return vl_wrap_void();
}

/* args: none
 * returns:
 *  - int
 */
vl_val sdl_get_tick(uint64_t argc, vl_val *argv)
{
  return vl_wrap_int(SDL_GetTicks());
}

/* args: 
     - fb: vector of floats [width * height]
 * returns:
 *  - void
 */
vl_val sdl_render_fb(uint64_t argc, vl_val *argv)
{
  const vl_vec *v;
  struct Pixel *fb;
  int pitch; /* width * sizeof(struct Pixel) */
  int width, height;
  int y, x;

  vl_check_arity(argc, 1);
  vl_check_type(argv[0], VL_VEC);

  v = vl_unwrap_vec(argv[0]);

  /* clear previous frame */
  SDL_RenderClear(renderer);

  /* render to texture directly */
  SDL_LockTexture(texture, NULL, (void **)&fb, &pitch);
  width = pitch / 4;
  height = v->len / width;
  for (y = 0; y < height; ++y) {
    for (x = 0; x < width; ++x) {
      int i = y*width + x;
      int shade = vl_unwrap_int(v->buf[i]);
      fb[i].r = shade;
      fb[i].g = shade;
      fb[i].b = shade;
      fb[i].a = 255;
    }
  }
  SDL_UnlockTexture(texture);

  /* texture -> renderer -> screen */
  SDL_RenderCopy(renderer, texture, NULL, NULL);
  SDL_RenderPresent(renderer);

  return vl_wrap_void();
}
