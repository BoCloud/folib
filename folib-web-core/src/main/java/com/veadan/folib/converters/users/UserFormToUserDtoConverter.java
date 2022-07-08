package com.veadan.folib.converters.users;

import com.veadan.folib.forms.users.UserForm;
import com.veadan.folib.users.dto.UserDto;

import org.springframework.core.convert.converter.Converter;

/**
 * @author Pablo Tirado
 * @author veadan
 */
public enum UserFormToUserDtoConverter
        implements Converter<UserForm, UserDto>
{

    INSTANCE;

    @Override
    public UserDto convert(UserForm userForm)
    {
        UserDto user = new UserDto();
        user.setUsername(userForm.getUsername());
        user.setPassword(userForm.getPassword());
        user.setEnabled(userForm.isEnabled());
        user.setRoleNames(userForm.getRoles());
        user.setSecurityTokenKey(userForm.getSecurityTokenKey());

        return user;
    }
}
