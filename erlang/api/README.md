# API Project

This project is an OTP application that provides an API to manage events. The API supports OPTIONS, GET, POST, PUT, and DELETE HTTP methods and handles JSON requests containing information about events in clinics.

## Project Structure

- **`src/`**: Contains the source code for the API.
- **`include/`**: Contains header files with record definitions.
- **`test/`**: Contains test suites for the API.
- **`Dockerfile`**: Docker configuration for building and running the application.
- **`Makefile`**: Makefile for building, running, testing, and cleaning the project.
- **`rebar.config`**: Configuration file for rebar3.
- **`Erlang-Onboarding.postman_collection.json`**: Postman collection for testing the API.

## Building the Project

To build the project, use the following command:

```sh
rebar3 compile
```

## Running the Project in Docker

To build and run the project in Docker, use the following commands:

```sh
make build
make run
```

This will build the Docker image and run the container, exposing the API on port 8080.

## Running Tests

The project includes tests written using Common Test. To run the tests, use the following command:

```sh
make test
```

This will execute the tests inside a Docker container.

## JSON Body for POST and PUT Requests

The JSON schema used to post and update records is as follows:

```json
{
  "event_type": "Arrival",
  "data": {
    "first_name": "Adam",
    "gender": "M",
    "last_name": "Everyman",
    "mrn": "99991946",
    "organization": "GPXS87a6lEnkmzONMeR2qIVg"
  }
}
```

## Testing with Postman

A Postman collection is included in the project to test the API endpoints. The collection file is `Erlang-Onboarding.postman_collection.json`.

To use the Postman collection:

1. Open Postman.
2. Import the `Erlang-Onboarding.postman_collection.json` file.
3. Set the `host` variable to the appropriate value (e.g., `localhost:8080`).
4. Run the requests in the collection to test the API.

## License

This project is licensed under the Apache License, Version 2.0. See the `LICENSE.md` file for details.