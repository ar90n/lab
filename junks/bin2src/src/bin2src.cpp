#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <iomanip>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/program_options.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/regex.hpp>

namespace po = boost::program_options;
namespace fs = boost::filesystem;

static const std::string work_dir_name = ".bin2src";
static const std::string template_ext = ".template";

int main(int argc, char const* argv[])
{
    po::options_description desc("Options");
    desc.add_options()
        ( "help,h", "Print this message and exit." )
        ( "list", "list of supported languages" )
        ( "lang,l", po::value<std::string>()->default_value("raw"), "type of language" )
        ( "line,L", po::value<int>()->default_value(INT_MAX), "data line width" )
        ( "name,n", po::value<std::string>()->default_value("data"), "name of variable" )
        ( "deli,d", po::value<std::string>()->default_value(","), "delimiter" )
    ;

    std::string input_file_path;
    po::variables_map vm;
    try
    {
        const auto parsing_result = po::parse_command_line( argc, argv, desc );
        po::store( parsing_result, vm );
        po::notify(vm);

        const auto unrecognized_options =  po::collect_unrecognized( parsing_result.options, po::collect_unrecognized_mode::include_positional );
        if( !unrecognized_options.empty() )
        {
            input_file_path = unrecognized_options.front();
        }
    }
    catch ( std::exception &e )
    {
        std::cerr << e.what() << std::endl;
        return -1;
    }

    if( vm.count("help") )
    {
        std::cerr << desc << std::endl;;
        return -1;
    }

    if( vm.count("list") )
    {
        std::cerr << "not supported" << std::endl;;
        return -1;
    }

    std::ifstream f( input_file_path );
    std::istream& input_stream = input_file_path.empty() ? std::cin : f;
    std::string lang = vm[ "lang" ].as< std::string >();
    std::string name_str = vm[ "name" ].as< std::string >();
    std::string delimiter = vm[ "deli" ].as< std::string >();
    int line_width = vm[ "line" ].as< int >();

    boost::system::error_code error;
    const fs::path home_dir_path( getenv("HOME") );
    const fs::path work_dir_path = home_dir_path / fs::path( work_dir_name );
    const bool result = fs::exists( work_dir_path, error );
    if( !result )
    {
        fs::create_directory( work_dir_path );
    }

    std::string template_str = "";
    const auto dir_file_range = boost::make_iterator_range( fs::directory_iterator( work_dir_path ), fs::directory_iterator() );
    for( auto template_file : dir_file_range )
    {
        if( !fs::is_directory( template_file ) )
        {
            fs::path curr_path( template_file );
            if( curr_path.stem() == lang && curr_path.extension() == template_ext )
            {
                std::ifstream ifs( curr_path.string() );
                template_str = std::string( std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>());
                ifs.close();
                break;
            }
        }
    }

    if( template_str.empty() )
    {
        std::cerr << lang << " is not support." << std::endl;
        return -1;
    }

    char c;
    int pos = 0;
    std::stringstream ss;
    while( input_stream.read( &c, 1 ) )
    {
        ss << std::hex << "0x" << std::setfill('0') << std::setw(2) << +(uint8_t)c << delimiter;
        if( ( ( pos + 1 ) % line_width ) == 0 )
        {
            ss << std::endl;
        }

        ++pos;
    }
    std::string data_str = ss.str();

    boost::regex r_name( "%NAME%" );
    template_str = boost::regex_replace( template_str, r_name, name_str, boost::format_all );
    boost::regex r_data( "%DATA%" );
    template_str = boost::regex_replace( template_str, r_data, data_str, boost::format_all );
    boost::regex r_escape( "%%" );
    template_str = boost::regex_replace( template_str, r_escape, "%", boost::format_all );

    std::cout << template_str << std::endl;

    return 0;
}
