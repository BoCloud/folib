package com.folib.client;

import com.folib.client.RestClient;
import org.junit.jupiter.api.Test;

class RestClientTest {

    @Test
    void getRestClientInstance() {
        RestClient.getRestClientInstance("http://127.0.0.1/api/ccc","admin","password");
        RestClient.getRestClientInstance("http://127.0.0.1:8080/api/ccc","admin","password");
        RestClient.getRestClientInstance("http://demo.folib.com/api/ccc","admin","password");
        RestClient.getRestClientInstance("https://demo.folib.com:8080/api/ccc","admin","password");
    }
}
