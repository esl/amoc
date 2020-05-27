### `./src/rest_api` directory description:
- `openapi.yaml` - OpenAPI definition of the Amoc REST API, to modify this file 
use [Swagger Editor](http://editor.swagger.io/).
- `amoc_rest` - generated erlang server (using [openapi generator](https://openapi-generator.tech/))
source code, do not change it manually without the real need.
### generation of the erlang server source code:
- source code can be generated using docker container:
  ```bash
  alias openapi-generator='docker run --rm -v "${PWD}:/local" openapitools/openapi-generator-cli:v4.3.1'
  ```
- validate the YAML spec first:
  ```bash
  openapi-generator validate -i /local/openapi.yaml
  ```
- generate the source code:
  ```bash
  openapi-generator generate -g erlang-server \
    --additional-properties=packageName=amoc_rest \
    -i /local/openapi.yaml -o /local/tmp
  ```
- copy OpenAPI JSON definition (`./tmp/priv/openapi.json`) into the `../../priv/` directory
- copy the source code from `./tmp/src/` into `./amoc_rest` directory (only `*.erl` files)
### known issues with the generated code:
ideally, the generated code should be used as is, without any changes. unfortunately that is not
possible, below is the list of the known issues fixed manually:
- issues with cowboy config preparation in `amoc_rest/amoc_rest_server.erl`, this one is already 
[reported](https://github.com/OpenAPITools/openapi-generator/issues/6354) to the openapi generator
- minor issues with spec definitions.
- minor issues with generated code for scenario (file) uploading method
- as we do not used REST API authentication in Amoc, we don't need `amoc_rest_auth.erl` file at all
### how does it work:
- the generated server extracts and verifies all the input parameters according to the YAML spec (including
JSON schema verification).
- if all parameters are correct, it triggers `amoc_rest_logic_handler:handle_request/3` function
- then it verifies the return value and respond to the client
- in case of any issues with input parameters or return value, the error is automatically reported
back to the client. this enforces sync of the YAML spec and implementation. 