#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/program_options.hpp>
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
        ( "name,n", po::value<std::string>()->default_value("data"), "name of variable" )
        ( "d", po::value<std::string>()->default_value(","), "delimiter" )
    ;

    po::variables_map vm;
    try
    {
        po::store(po::parse_command_line( argc, argv, desc ), vm );
        po::notify(vm);
    }
    catch ( std::exception &e )
    {
        std::cerr << e.what() << std::endl;
        return -1;
    }

    if( vm.count("help") )
    {
        std::cerr << desc << "\n";
        return -1;
    }

    if( vm.count("list") )
    {
        std::cerr << "not supported" << "\n";
        return -1;
    }

    std::string lang = vm[ "lang" ].as< std::string >();
    std::string name_str = vm[ "name" ].as< std::string >();
    std::string delimiter = vm[ "d" ].as< std::string >();

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

    std::stringstream ss;
    char c;
    while( std::cin.read( &c, 1 ) )
    {
        ss << std::hex << "0x" << +(uint8_t)c << ',';
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
