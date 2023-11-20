package com.veadan.folib.forms.configuration;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonSerialize(include = JsonSerialize.Inclusion.ALWAYS)
public class ServerSettingsForm
{

    @NotBlank(message = "The name of this instance")
    @JsonProperty
    private String instanceName;

    @NotBlank(message = "A base URL must be specified.")
    @JsonProperty
    private String baseUrl;

    @NotNull(message = "A port must be specified.")
    @Min(value = 1, message = "The port number must be an integer between 1 and 65535.")
    @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.")
    @JsonProperty
    private Integer port;

    /**
     * 节点传输速率（KB/s）
     */
    private Long kbps;
    /**
     * 节点传输切片大小（MB）
     */
    private Long sliceMbSize;

    @Valid
    @JsonProperty
    private AdvancedConfigurationForm advancedConfigurationForm = new AdvancedConfigurationForm();

    @Valid
    @JsonProperty
    private CorsConfigurationForm corsConfigurationForm = new CorsConfigurationForm();

    @Valid
    @JsonProperty
    private SmtpConfigurationForm smtpConfigurationForm = new SmtpConfigurationForm();

    @Valid
    @JsonProperty
    private ProxyConfigurationForm proxyConfigurationForm = new ProxyConfigurationForm();

    public ServerSettingsForm()
    {
    }

    public ServerSettingsForm(@NotBlank(message = "A base URL must be specified.") String baseUrl,
                              @NotNull(message = "A port must be specified.")
                              @Min(value = 1, message = "The port number must be an integer between 1 and 65535.")
                              @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.") Integer port)
    {
        this.baseUrl = baseUrl;
        this.port = port;
    }

    public ServerSettingsForm(String baseUrl,
                              Integer port,
                              String instanceName,
                              CorsConfigurationForm corsConfigurationForm,
                              SmtpConfigurationForm smtpConfigurationForm,
                              ProxyConfigurationForm proxyConfigurationForm)
    {
        this.baseUrl = baseUrl;
        this.port = port;
        this.instanceName = instanceName;
        this.corsConfigurationForm = corsConfigurationForm;
        this.smtpConfigurationForm = smtpConfigurationForm;
        this.proxyConfigurationForm = proxyConfigurationForm;
    }

    public String getBaseUrl()
    {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl)
    {
        this.baseUrl = baseUrl;
    }

    public Integer getPort()
    {
        return port;
    }

    public void setPort(Integer port)
    {
        this.port = port;
    }

    public Long getKbps() {
        return kbps;
    }

    public void setKbps(Long kbps) {
        this.kbps = kbps;
    }

    public Long getSliceMbSize() {
        return sliceMbSize;
    }

    public void setSliceMbSize(Long sliceMbSize) {
        this.sliceMbSize = sliceMbSize;
    }

    public String getInstanceName()
    {
        return instanceName;
    }

    public void setInstanceName(String instanceName)
    {
        this.instanceName = instanceName;
    }

    public CorsConfigurationForm getCorsConfigurationForm()
    {
        return corsConfigurationForm;
    }

    public void setCorsConfigurationForm(CorsConfigurationForm corsConfigurationForm)
    {
        this.corsConfigurationForm = corsConfigurationForm;
    }

    public SmtpConfigurationForm getSmtpConfigurationForm()
    {
        return smtpConfigurationForm;
    }

    public void setSmtpConfigurationForm(SmtpConfigurationForm smtpConfigurationForm)
    {
        this.smtpConfigurationForm = smtpConfigurationForm;
    }

    public ProxyConfigurationForm getProxyConfigurationForm()
    {
        return proxyConfigurationForm;
    }

    public void setProxyConfigurationForm(ProxyConfigurationForm proxyConfigurationForm)
    {
        this.proxyConfigurationForm = proxyConfigurationForm;
    }

    public AdvancedConfigurationForm getAdvancedConfigurationForm() {
        return advancedConfigurationForm;
    }

    public void setAdvancedConfigurationForm(AdvancedConfigurationForm advancedConfigurationForm) {
        this.advancedConfigurationForm = advancedConfigurationForm;
    }
}
