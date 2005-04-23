use Module::Build;
Module::Build->new(
                   module_name => 'CAM::XML',
                   license => 'perl',
                   requires => {
                      'XML::Parser'              => '2.27',
                   },
                   build_requires => {
                      'Test::More'               => '0.01',
                   },
                   )->create_build_script;
