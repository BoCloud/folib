package com.veadan.folib.licence;

import com.alibaba.fastjson.JSONObject;

import java.io.Serializable;

public class ActivateVo implements Serializable {


    private String mac;
    private boolean haveError;
    private boolean dalyOut;
    private String level;
    private JSONObject object;

    public String getMac() {
        return mac;
    }

    public void setMac(String mac) {
        this.mac = mac;
    }

    public boolean isHaveError() {
        return haveError;
    }

    public void setHaveError(boolean haveError) {
        this.haveError = haveError;
    }

    public boolean isDalyOut() {
        return dalyOut;
    }

    public void setDalyOut(boolean dalyOut) {
        this.dalyOut = dalyOut;
    }

    public JSONObject getObject() {
        return object;
    }

    public void setObject(JSONObject object) {
        this.object = object;
    }

    public String getLevel() {
        return level;
    }

    public void setLevel(String level) {
        this.level = level;
    }
}
