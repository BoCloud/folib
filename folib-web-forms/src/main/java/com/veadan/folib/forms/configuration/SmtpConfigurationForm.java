package com.veadan.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.veadan.folib.configuration.MutableSmtpConfiguration;
import com.veadan.folib.configuration.SmtpConfiguration;

import javax.validation.constraints.*;
import java.io.Serializable;
import java.util.Optional;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SmtpConfigurationForm {

    @NotBlank(message = "An SMTP host must be provided.", groups = SmtpConfigurationFormChecks.class)
    private String host;

    @NotNull(message = "The SMTP port must be provided.", groups = SmtpConfigurationFormChecks.class)
    @Min(value = 1, message = "The port number must be an integer between 1 and 65535.", groups = SmtpConfigurationFormChecks.class)
    @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.", groups = SmtpConfigurationFormChecks.class)
    private Integer port;

    private String username;

    private String password;

    @Pattern(regexp = "plain|ssl|tls",
            flags = Pattern.Flag.CASE_INSENSITIVE,
            message = "Please, set a valid SMTP connection type.",
            groups = SmtpConfigurationFormChecks.class)
    private String connection;

    public SmtpConfigurationForm() {
    }

    public SmtpConfigurationForm(String host,
                                 Integer port,
                                 String connection,
                                 String username,
                                 String password) {
        this.host = host;
        this.port = port;
        this.connection = connection;
        this.username = username;
        this.password = password;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public Integer getPort() {
        return port;
    }

    public void setPort(Integer port) {
        this.port = port;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getConnection() {
        return connection;
    }

    public void setConnection(String connection) {
        this.connection = connection;
    }

    @JsonIgnore()
    public MutableSmtpConfiguration getMutableSmtpConfiguration() {
        return new MutableSmtpConfiguration(this.host,
                this.port,
                this.connection,
                this.username,
                this.password);
    }

    @JsonIgnore()
    public static SmtpConfigurationForm fromConfiguration(SmtpConfiguration source) {
        SmtpConfiguration configuration = Optional.ofNullable(source).orElse(
                new SmtpConfiguration(new MutableSmtpConfiguration())
        );

        return new SmtpConfigurationForm(configuration.getHost(),
                configuration.getPort(),
                configuration.getConnection(),
                configuration.getUsername(),
                configuration.getPassword());
    }

    public interface SmtpConfigurationFormChecks
            extends Serializable {
        // validation group marker interface for fields.
    }

}
