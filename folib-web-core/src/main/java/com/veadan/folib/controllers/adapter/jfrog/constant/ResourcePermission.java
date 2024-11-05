package com.veadan.folib.controllers.adapter.jfrog.constant;

public enum ResourcePermission {

    READ("r","ARTIFACTS_RESOLVE"),

    WRITE("w","ARTIFACTS_DEPLOY"),

    DELETE("d", "ARTIFACTS_DELETE");

    //todo 未适配jfrog m,n

    //jfrog权限
    private final String jPermission;
    //folib权限
    private final String fPermission;

    ResourcePermission(String jPermission, String fPermission) {
        this.jPermission = jPermission;
        this.fPermission = fPermission;
    }
    public String getJPermission() {
        return jPermission;
    }
    public String getFPermission() {
        return fPermission;
    }
    public static ResourcePermission getByFPermission(String fPermission){
        for(ResourcePermission permission : ResourcePermission.values()){
            if(permission.getFPermission().equals(fPermission)){
                return permission;
            }
        }
        return null;
    }
    public static ResourcePermission getByJPermission(String jPermission){
        for(ResourcePermission permission : ResourcePermission.values()){
            if(permission.getJPermission().equals(jPermission)){
                return permission;
            }
        }
        return null;
    }
}
