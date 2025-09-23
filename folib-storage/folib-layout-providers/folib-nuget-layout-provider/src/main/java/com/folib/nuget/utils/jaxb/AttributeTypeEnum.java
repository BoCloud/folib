package com.folib.nuget.utils.jaxb;


public enum AttributeTypeEnum {
    BOOLEAN("Edm.Boolean"),
    DATE_TIME("Edm.DateTime"),
    INT_64("Edm.Int64"),
    TEXT("text"),
    TRUE("true");

    public final String type;

    private AttributeTypeEnum(String label) {
        this.type = label;
    }

    // $FF: synthetic method
    private static AttributeTypeEnum[] $values() {
        return new AttributeTypeEnum[]{BOOLEAN, DATE_TIME, INT_64, TEXT, TRUE};
    }
}
