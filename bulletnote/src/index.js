import { Elm } from './Main.elm';
import buildString from './build.js';

/***** Constants *****/
const loginKey = 'mtnew';
// login expires after 30 minutes
const timeout = 30 * 60 * 1000;

function isLoggedIn() {
  return (
    Math.abs(new Date() - Number(localStorage.getItem(loginKey))) < timeout
  );
}

/***** App initialization *****/
const app = Elm.Main.init({
  flags: isLoggedIn(),
});

/***** Login and logout logic *****/
app.ports.loginPort.subscribe(() => {
  localStorage.setItem(loginKey, Number(new Date()));
});
app.ports.logoutPort.subscribe(() => {
  localStorage.removeItem(loginKey);
});

/***** Lazy loading logic *****/
function loadImage(image) {
  if (image.dataset && image.dataset.src) {
    image.src = image.dataset.src;
    image.style['min-height'] = '50px';
    image.style.height = '';
  }
}
function checkLazyLoad() {
  const images = document.querySelectorAll('.lazy-load');
  if ('IntersectionObserver' in window) {
    // IntersectionObserver Supported
    const observer = new IntersectionObserver(
      (changes, observer) => {
        changes.forEach(change => {
          if (change.intersectionRatio > 0) {
            loadImage(change.target);
            observer.unobserve(change.target);
          }
        });
      },
      {
        root: null,
        rootMargin: '0px',
        threshold: 0.5,
      }
    );
    images.forEach(img => observer.observe(img));
  } else {
    // IntersectionObserver NOT Supported
    images.forEach(image => loadImage(image));
  }
}
app.ports.startLazyPort.subscribe(() => setTimeout(checkLazyLoad, 50));

/***** Scroll to image in page *****/
function scrollTo(image) {
  const images = document.querySelectorAll('img');
  // look for the image with the right name in its src (or data-src)
  const scrollImages = Array.from(images)
    .map(img => ({ src: img.src + img.dataset.src, img }))
    .filter(({ src }) => src.includes(image));
  // if such an image exists, scroll to it
  if (scrollImages.length === 1) {
    scrollImages[0].img.scrollIntoView();
  }
}
app.ports.scrollToPort.subscribe(img => setTimeout(() => scrollTo(img), 100));

/***** Scroll to top of the page *****/
function scrollToTop() {
  const c = document.documentElement.scrollTop || document.body.scrollTop;
  if (c > 0) {
    window.requestAnimationFrame(scrollToTop);
    window.scrollTo(0, c - c / 8);
  }
}
app.ports.scrollTopPort.subscribe(() => setTimeout(scrollToTop, 50));

/***** Big side scroller and show/hide it*****/
function initBigScroller() {
  try {
    const el = document.querySelector('#big-scroller');
    const navMenu = document.querySelector('#nav-menu');
    const rem = parseFloat(getComputedStyle(document.documentElement).fontSize);
    // max for scroller top
    const scrollerMax = window.innerHeight - 12 * rem;
    // initialize scroller to correct position in the page
    el.style.top =
      (window.scrollY / document.body.scrollHeight) * scrollerMax + 'px';
    // speed at which to show/hide scroller
    const scrollSpeed = 10;

    let prevScroll = window.scrollY;
    let draggingScroller = false;

    function hideScroller() {
      el.style.left = '-4rem';
      navMenu.style.bottom = '-4.5rem';
    }
    function showScroller() {
      el.style.left = '0px';
      navMenu.style.bottom = '0px';
    }

    // listen to scroll
    document.onscroll = e => {
      // do not interfere with scroller
      if (draggingScroller) return;

      const curScroll = window.scrollY;
      // update big scroller position
      el.style.top =
        (window.scrollY / document.body.scrollHeight) * scrollerMax + 'px';

      // hide scroller and other things if scrolling down
      if (curScroll - prevScroll > scrollSpeed) {
        hideScroller();
      } else if (curScroll - prevScroll < -scrollSpeed) {
        showScroller();
      }

      prevScroll = curScroll;
    };

    let touchStart = 0;
    let scrollerStart = 0;
    let timeout = null;
    el.addEventListener('touchstart', e => {
      e.preventDefault();

      // where touch and scroller started at
      touchStart = e.touches[0].clientY;
      scrollerStart = Number(el.style.top.slice(0, -2));
      draggingScroller = true;
      timeout = null;
    });

    el.addEventListener('touchmove', e => {
      e.preventDefault();
      // throttle to 500ms
      if (timeout) {
        clearTimeout(timeout);
      }
      // new position of scroller
      const newPos = scrollerStart + (e.touches[0].clientY - touchStart);
      // move scroller
      if (newPos > 0 && newPos < scrollerMax) {
        el.style.top = newPos + 'px';
      }
      timeout = setTimeout(() => scrollToScroller(newPos), 200);
    });
    // scroll to position of scroller
    function scrollToScroller(newPos) {
      timeout = null;
      window.scrollTo({
        top: (newPos / scrollerMax) * document.body.scrollHeight,
        // behavior: 'smooth',
      });
      setTimeout(showScroller, 10);
    }

    el.addEventListener('touchend', () => {
      draggingScroller = false;
    });
  } catch (e) {
    console.warn('Error occured in initBigScroller');
  }
}
app.ports.initScrollerPort.subscribe(() =>
  setTimeout(() => initBigScroller(), 50)
);

/***** Get the build string *****/
app.ports.getBuildPort.subscribe(() =>
  app.ports.receiveBuildPort.send(buildString)
);


/***** Setup the key listener for the login screen *****/
function loginKeyListener() {
  document.addEventListener('keydown', e => {
    const num = Number(e.key)
    if (!isNaN(num)) {
      document.querySelector(`button[data-number="${num}"]`).click();
    }
  })
}
loginKeyListener();
