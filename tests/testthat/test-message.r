context("message")

rusti = function(x){
    rust(code = x ,depend = '
     [dependencies]
     rustr = {git = "https://github.com/rustr/rustr.git"}')
}

test_that("test message",{
    skip_on_cran()
    rusti('
    // #[rustr_export]
    pub fn say_warning(){
        r_warn("test");
    }')
    expect_warning(say_warning(), "test")
})