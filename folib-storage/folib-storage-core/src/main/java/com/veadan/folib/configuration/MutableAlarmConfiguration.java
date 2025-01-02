package com.veadan.folib.configuration;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;
import java.util.List;

public class MutableAlarmConfiguration implements Serializable {


    //通知策略
    private List<String> notificationPolicy;

    //指定用户列表
    private List<String> recipients;

    //指定邮箱列表
    private List<String>  emails;

    private double storageThreshold;

    @JsonCreator
    public MutableAlarmConfiguration(){
    }

    @JsonCreator
    public MutableAlarmConfiguration(@JsonProperty("notificationPolicy") List<String> notificationPolicy,
                                     @JsonProperty("recipients") List<String> recipients,
                                     @JsonProperty("emails") List<String> emails,
                                     @JsonProperty("storageThreshold") double storageThreshold) {
        this.notificationPolicy = notificationPolicy;
        this.recipients = recipients;
        this.emails = emails;
        this.storageThreshold = storageThreshold;
    }

    public List<String> getNotificationPolicy() {
        return notificationPolicy;
    }

    public void setNotificationPolicy(List<String> notificationPolicy) {
        this.notificationPolicy = notificationPolicy;
    }

    public List<String> getRecipients() {
        return recipients;
    }

    public void setRecipients(List<String> recipients) {
        this.recipients = recipients;
    }

    public List<String> getEmails() {
        return emails;
    }

    public void setEmails(List<String> emails) {
        this.emails = emails;
    }

    public double getStorageThreshold() {
        return storageThreshold;
    }

    public void setStorageThreshold(double storageThreshold) {
        this.storageThreshold = storageThreshold;
    }
}
