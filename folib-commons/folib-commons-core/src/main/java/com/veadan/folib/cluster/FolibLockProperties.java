package com.veadan.folib.cluster;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Component
public class FolibLockProperties {

    @Value("${folib.distributed.lockip}")
    private String folibLockIp;

    public String getFolibLockIp() {
        return folibLockIp;
    }

    public void setFolibLockIp(String folibLockIp) {
        this.folibLockIp = folibLockIp;
    }
}
