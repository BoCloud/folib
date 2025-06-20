package com.veadan.folib.converters.users;

import com.veadan.folib.dto.users.UserDto;

import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 * @author veadan
 */
public enum UserFormToUserDtoConverter
        implements Converter<UserDto, com.veadan.folib.users.dto.UserDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.users.dto.UserDto convert(UserDto userForm)
    {
        com.veadan.folib.users.dto.UserDto user = new com.veadan.folib.users.dto.UserDto();
        user.setUsername(userForm.getUsername());
        user.setPassword(userForm.getPassword());
        user.setOriginalPassword(userForm.getOriginalPassword());
        user.setEnabled(userForm.isEnabled());
        user.setRoleNames(userForm.getRoles());
        user.setSecurityTokenKey(userForm.getSecurityTokenKey());
        user.setEmail(userForm.getEmail());

        return user;
    }
}
