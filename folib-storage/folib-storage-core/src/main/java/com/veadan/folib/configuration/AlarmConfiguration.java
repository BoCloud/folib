package com.veadan.folib.configuration;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;

import javax.annotation.concurrent.Immutable;
import java.util.List;

@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
public class AlarmConfiguration {


    @XmlAttribute
    private List<String> notificationPolicy;

    @XmlAttribute
    private List<String> recipients;

    @XmlAttribute
    private List<String>  emails;

    @XmlAttribute
    private double storageThreshold;

    public AlarmConfiguration() {
        // Default constructor
    }

    // Constructor
    public AlarmConfiguration(MutableAlarmConfiguration config) {
        this.notificationPolicy = config.getNotificationPolicy();
        this.recipients = config.getRecipients();
        this.emails = config.getEmails();
        this.storageThreshold = config.getStorageThreshold();
    }

    // Getters and setters
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

