package com.veadan.folib.authentication.api.ldap;

import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * Test class of CustomLdapUserDetailsMapper
 * @author mbharti
 * @date 17/11/20
 */
public class CustomLdapUserDetailsMapperTest
{

    @Test
    public void testMapPasswordBase64EncodedFalse()
    {
        CustomLdapUserDetailsMapper userDetailsMapper = new CustomLdapUserDetailsMapper();
        userDetailsMapper.setUserPasswordEncoded(false);
        String encodedPassword = userDetailsMapper.mapPassword("{MD5}X03MO1qnZdYdgyfeuILPmQ==");

        assertThat(encodedPassword).isEqualTo("{MD5}X03MO1qnZdYdgyfeuILPmQ==");

        encodedPassword = userDetailsMapper.mapPassword("e2JjcnlwdH0kMmEkMTAkbHB3bHh5anZYS3pOMWNjQ3J3MlBCdVp4LmVWZXNXYmZtVGJzckNib01VLmdzTldWY1pXTWk=");

        assertThat(encodedPassword).isEqualTo("e2JjcnlwdH0kMmEkMTAkbHB3bHh5anZYS3pOMWNjQ3J3MlBCdVp4LmVWZXNXYmZtVGJzckNib01VLmdzTldWY1pXTWk=");

        encodedPassword = userDetailsMapper.mapPassword("{SHA-256}{mujKRdqeWWYAWhczNwVnBl6L6dHNwWO5eIGZ/G7pnBg=}bb63813f5b6f64ae306ebbbb23dcbb1c6f49eb9b989fc466b1b1a24a011bb2ce");

        assertThat(encodedPassword).isEqualTo("{SHA-256}{mujKRdqeWWYAWhczNwVnBl6L6dHNwWO5eIGZ/G7pnBg=}bb63813f5b6f64ae306ebbbb23dcbb1c6f49eb9b989fc466b1b1a24a011bb2ce");
    }

    @Test
    public void testMapPasswordBase64EncodedTrue()
    {
        CustomLdapUserDetailsMapper userDetailsMapper = new CustomLdapUserDetailsMapper();
        userDetailsMapper.setUserPasswordEncoded(true);
        String encodedPassword = userDetailsMapper.mapPassword("{MD5}X03MO1qnZdYdgyfeuILPmQ==");

        assertThat(encodedPassword).isEqualTo("{MD5}5f4dcc3b5aa765d61d8327deb882cf99");

        encodedPassword = userDetailsMapper.mapPassword("e2JjcnlwdH0kMmEkMTAkbHB3bHh5anZYS3pOMWNjQ3J3MlBCdVp4LmVWZXNXYmZtVGJzckNib01VLmdzTldWY1pXTWk=");

        assertThat(encodedPassword).isEqualTo("{bcrypt}$2a$10$lpwlxyjvXKzN1ccCrw2PBuZx.eVesWbfmTbsrCboMU.gsNWVcZWMi");
    }
}
