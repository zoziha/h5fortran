from conans import ConanFile, CMake


class H5fortranConan(ConanFile):
    name = "h5fortran"
    version = "2.9.0"
    license = "MIT"  # noqa: A003
    url = "https://github.com/scivision/h5fortran"
    requires = "hdf5/[>=1.10]"
    build_policy = "missing"
    description = "Easy, thin, robust polymorphic Fortran HDF5 interface"
    settings = "os", "compiler", "build_type", "arch"
    options = {"shared": [True, False]}
    default_options = {"shared": False}
    generators = "cmake"

    def source(self):
        self.run("git clone https://github.com/scivision/h5fortran.git")

    def build(self):
        cmake = CMake(self)
        args = f'-DCMAKE_INSTALL_PREFIX="{self.package_folder}"'
        print(cmake.command_line)
        self.run(f"cmake {self.source_folder} {cmake.command_line} {args}")
        self.run(f"cmake --build . --target install")