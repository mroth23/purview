$(function () {
    $('#featured_slide').after('<div id="fsn"><ul id="fs_pagination"></ul></div>').cycle({
        timeout: 5000,
        fx: 'fade',
        pager: '#fs_pagination',
        pause: 0,
        pauseOnPagerHover: 0
    });
});