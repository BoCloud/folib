package com.veadan.folib.dto.configuration;

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
public class ServerSettingsDto
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
    @Min(value = 1, message = "The kbps be an integer between 1 and 999999.")
    @Max(value = 999999, message = "The kbps be an integer between 1 and 999999.")
    private Integer kbps;
    /**
     * 节点传输切片大小（MB）
     */
    @Min(value = 1, message = "The sliceMbSize must be an integer between 1 and 10240.")
    @Max(value = 10240, message = "The sliceMbSize must be an integer between 1 and 10240.")
    private Long sliceMbSize;

    @Valid
    @JsonProperty
    private AdvancedConfigurationDto advancedConfigurationForm = new AdvancedConfigurationDto();

    @Valid
    @JsonProperty
    private CorsConfigurationDto corsConfigurationForm = new CorsConfigurationDto();

    @Valid
    @JsonProperty
    private SmtpConfigurationDto smtpConfigurationForm = new SmtpConfigurationDto();

    @Valid
    @JsonProperty
    private ProxyConfigurationDto proxyConfigurationForm = new ProxyConfigurationDto();

    @Valid
    @JsonProperty
    private AlarmConfigurationDto alarmConfigurationForm = new AlarmConfigurationDto();

    public ServerSettingsDto()
    {
    }

    public ServerSettingsDto(@NotBlank(message = "A base URL must be specified.") String baseUrl,
                             @NotNull(message = "A port must be specified.")
                              @Min(value = 1, message = "The port number must be an integer between 1 and 65535.")
                              @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.") Integer port)
    {
        this.baseUrl = baseUrl;
        this.port = port;
    }

    public ServerSettingsDto(String baseUrl,
                             Integer port,
                             String instanceName,
                             CorsConfigurationDto corsConfigurationForm,
                             SmtpConfigurationDto smtpConfigurationForm,
                             ProxyConfigurationDto proxyConfigurationForm,
                             AlarmConfigurationDto alarmConfigurationForm)
    {
        this.baseUrl = baseUrl;
        this.port = port;
        this.instanceName = instanceName;
        this.corsConfigurationForm = corsConfigurationForm;
        this.smtpConfigurationForm = smtpConfigurationForm;
        this.proxyConfigurationForm = proxyConfigurationForm;
        this.alarmConfigurationForm = alarmConfigurationForm;
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

    public Integer getKbps() {
        return kbps;
    }

    public void setKbps(Integer kbps) {
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

    public CorsConfigurationDto getCorsConfigurationForm()
    {
        return corsConfigurationForm;
    }

    public void setCorsConfigurationForm(CorsConfigurationDto corsConfigurationForm)
    {
        this.corsConfigurationForm = corsConfigurationForm;
    }

    public SmtpConfigurationDto getSmtpConfigurationForm()
    {
        return smtpConfigurationForm;
    }

    public void setSmtpConfigurationForm(SmtpConfigurationDto smtpConfigurationForm)
    {
        this.smtpConfigurationForm = smtpConfigurationForm;
    }

    public ProxyConfigurationDto getProxyConfigurationForm()
    {
        return proxyConfigurationForm;
    }

    public void setProxyConfigurationForm(ProxyConfigurationDto proxyConfigurationForm)
    {
        this.proxyConfigurationForm = proxyConfigurationForm;
    }

    public AdvancedConfigurationDto getAdvancedConfigurationForm() {
        return advancedConfigurationForm;
    }

    public void setAdvancedConfigurationForm(AdvancedConfigurationDto advancedConfigurationForm) {
        this.advancedConfigurationForm = advancedConfigurationForm;
    }

    public AlarmConfigurationDto getAlarmConfigurationForm() {
		return alarmConfigurationForm;
	}
    public void setAlarmConfigurationForm(AlarmConfigurationDto alarmConfigurationForm) {
		this.alarmConfigurationForm = alarmConfigurationForm;
	}
}
