#include <iostream>
#include <string.h>
#include "sass_context.h"

using namespace std;

int main( int argc, const char* argv[] ) {
    if (argc != 3) {
        cout << R"(
{
  "status: 5,
  "message": Invalid number of parameters
}
)" << endl;
        return 5;
    }

    // get the input sass files
    const char* tmpInput = argv[1];
    char* input = new char[strlen(tmpInput)];
    strcpy(input, tmpInput);

    // get the input path
    //const char* input_path = argv[2];

    // get the output path
    const char* output_path = argv[2];

    // get the include path
    //const char* include_path = argv[3];

    // the sourcemap file
    char* source_map_file = new char[strlen(output_path) + 3];
    strcpy(source_map_file, output_path);
    strcat(source_map_file, ".map");

    //Create the file context and get all related structs
    struct Sass_File_Context* file_context = sass_make_file_context(input);
    struct Sass_Context* context = sass_file_context_get_context(file_context);
    struct Sass_Options* options = sass_file_context_get_options(file_context);

    //Set options
    sass_option_set_precision(options, 10);
    sass_option_set_source_comments(options, false);
    sass_file_context_set_options(file_context, options);
    sass_option_set_input_path(options, input);
    sass_option_set_output_path(options, output_path);
    sass_option_set_source_map_embed(options,true);
    sass_option_set_source_map_contents(options,true);
    sass_option_set_omit_source_map_url(options,false);
    //sass_option_set_include_path (options, include_path);
    sass_option_set_source_map_file(options, source_map_file);

    sass_compile_file_context(file_context);

    const char* output = sass_context_get_output_string(context);
    const char* source_map_output = sass_context_get_source_map_string(context);

    int error_status = sass_context_get_error_status(context);
    const char* json_error = sass_context_get_error_json(context);

    if (error_status > 0) {
        cout << json_error << endl;
    } else {
        cout << output << endl;
        //cout << source_map_output << endl;
    }

    // Release memory dedicated to the C compiler
    sass_delete_file_context(file_context);
    delete source_map_file;

    // exit status
    return error_status;
}