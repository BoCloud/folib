package com.veadan.folib.dto.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.veadan.folib.configuration.AlarmConfiguration;
import com.veadan.folib.configuration.MutableAlarmConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;
import java.util.Optional;

/**
 * 告警配置表单
 */
@Data
@Accessors(chain = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AlarmConfigurationDto {

    //通知策略
    private List<String> notificationPolicy;

    //指定用户列表
    private List<String> recipients;

    //指定邮箱列表
    private List<String>  emails;

    private String cronExpression;

    private double storageThreshold;

    public AlarmConfigurationDto(List<String> notificationPolicy, List<String> recipients, List<String> emails, double storageThreshold) {
        this.notificationPolicy = notificationPolicy;
        this.recipients = recipients;
        this.emails = emails;
        this.storageThreshold = storageThreshold;
    }

    @JsonIgnore()
    public AlarmConfiguration toAlarmConfiguration() {
        return new AlarmConfiguration(new MutableAlarmConfiguration(notificationPolicy, recipients, emails, storageThreshold));
    }

    @JsonIgnore()
    public MutableAlarmConfiguration getMutableAlarmConfiguration() {
        return new MutableAlarmConfiguration(notificationPolicy, recipients, emails,storageThreshold);
    }
    @JsonIgnore()
    public static AlarmConfigurationDto formConfiguration(AlarmConfiguration source) {
        AlarmConfiguration configuration = Optional.ofNullable(source).orElse(new AlarmConfiguration(new MutableAlarmConfiguration()));
        return new AlarmConfigurationDto(configuration.getNotificationPolicy(), configuration.getRecipients(), configuration.getEmails(), configuration.getStorageThreshold());

    }

}