<template>
  <div class="personal">
    <a-row type="flex" :gutter="[24, 24]">
     <!-- <a-col :span="24" :lg="6">
        <a-affix :offset-top="navbarFixed ? 100 : 10">
          <a-card :bordered="false" class="header-solid mb-24">
            <a-anchor :targetOffset="navbarFixed ? 100 : 10" :affix="false" @click="handleClick">
              <a-anchor-link href="#personal">
                <div slot="title" class="ant-list-item-meta">
                  <a-icon type="snippets" theme="filled" class="text-gray-6 text-lg" />
                  <h4 class="ant-list-item-meta-title">
                    <span class="font-regular">个人信息</span>
                  </h4>
                </div>
              </a-anchor-link>
              <a-anchor-link href="#password">
                <div slot="title" class="ant-list-item-meta">
                  <a-icon type="unlock" theme="filled" class="text-gray-6 text-lg" />
                  <h4 class="ant-list-item-meta-title">
                    <span class="font-regular">密码修改</span>
                  </h4>
                </div>
              </a-anchor-link>
            </a-anchor>
          </a-card>
        </a-affix>
      </a-col> -->
      <a-col :span="24" :lg="18">
        <!-- personal Info card -->
        <a-card :bordered="false" id="personal" class="header-solid mb-24">
          <template #title>
            <h5 class="mb-0 font-semibold">{{ $t('Users.PersonalInformation') }}</h5>
          </template>
          <a-form-model
            ref="personalForm"
            :model="personalForm"
            :rules="personalRules"
            :hideRequiredMark="true"
          >
            <a-row :gutter="[24]">
              <a-col :span="24" :md="4" class="text-center">
                <div class="avatar-upload">
                    <svg class="icon folib-avatar" :style="{ fontSize: '32px' }" aria-hidden="true">
                      <use :xlink:href="'#'+ personalForm.avatar"></use>
                    </svg>
                    <a-button icon="edit" @click="showAvatarModal"></a-button>
                </div>
              </a-col>
              <a-col :span="24" :md="20">
                <a-row :gutter="[24]">
                  <a-col :span="24" :md="8">
                    <a-form-model-item class="mb-10" :label="$t('Users.UserName')" :colon="false" prop="username">
                      <a-input v-model="personalForm.username" :disabled="true" :placeholder="$t('Users.EnterYourUsername')" />
                    </a-form-model-item>
                    <a-form-model-item class="mb-10" :label="$t('Users.Email')" :colon="false">
                      <a-input v-model="personalForm.email" :placeholder="$t('Users.EnterEmail')" />
                    </a-form-model-item>
                    <a-form-model-item class="mb-10" :label="$t('Users.NewPassword')" :colon="false" prop="password">
                      <a-input-password autocomplete="new-password" v-model="personalForm.password" :placeholder="$t('Users.EnterNewPassword')" />
                    </a-form-model-item>
                    <a-form-model-item class="mb-10" :label="$t('Users.EnterYourNewPasswordAgain')" :colon="false" prop="againPassword">
                      <a-input-password autocomplete="new-password" v-model="personalForm.againPassword" :placeholder="$t('Users.EnterNewPasswordAgain')" />
                    </a-form-model-item>
                  </a-col>
                </a-row>
              </a-col>
            </a-row>
            <a-form-model-item :wrapper-col="{ span: 14, offset: 6 }">
              <a-button type="primary" @click="personalFormSubmit">
                {{ $t('Users.Save') }}
              </a-button>
              <a-button class="ml-10" @click="personalResetForm">
                {{ $t('Users.Cancel') }}
              </a-button>
            </a-form-model-item>
          </a-form-model>
        </a-card>
        <!-- / personal Info card -->

        <!-- password card -->
        <!-- <a-card :bordered="false" id="password" class="header-solid mb-24">
          <template #title>
            <h5 class="mb-0 font-semibold">密码修改</h5>
          </template>
          <a-form-model
            class="ml-10"
            ref="passwordForm"
            :model="passwordForm"
            :rules="passwordRules"
            :hideRequiredMark="true"
          >
            <a-row :gutter="[24]">
              <a-col :span="24" :md="20">
                <a-row :gutter="[24]">
                  <a-col :span="24" :md="8">
                    <a-form-model-item class="mb-10" label="新密码" :colon="false" prop="password">
                      <a-input-password autocomplete="new-password" v-model="passwordForm.password" placeholder="请输入新密码" />
                    </a-form-model-item>
                    <a-form-model-item class="mb-10" label="再次输入新密码" :colon="false" prop="againPassword">
                      <a-input-password autocomplete="new-password" v-model="passwordForm.againPassword" placeholder="请再次输入新密码" />
                    </a-form-model-item>
                  </a-col>
                </a-row>
              </a-col>
            </a-row>
            <a-form-model-item :wrapper-col="{ span: 14, offset: 6 }">
              <a-button type="primary" @click="passwordFormSubmit">
                保存
              </a-button>
              <a-button class="ml-10" @click="passwordResetForm">
                取消
              </a-button>
            </a-form-model-item>
          </a-form-model>
        </a-card> -->
        <!-- / password card -->
      </a-col>
    </a-row>

    <a-modal v-model="showAvatarSelector" width="60%" height="50%" :footer="null" :forceRender="true" title="">
      <AvatarSelector @avatarChange="avatarChange"></AvatarSelector>
    </a-modal>
  </div>
</template>
<script>
import {
  getInfo,
  updateUser
} from "@/api/login"
import AvatarSelector from "@/components/AvatarSelector/AvatarSelector.vue"
import { encrypt } from "@/utils/jsencrypt"

export default {
  inject: ["reload"],
  props: {
    navbarFixed: {
			type: Boolean,
			default: false,
		},
	},
  data() {
    const checkPassword = (rule, value, callback) => {
      if (value && value.length > 0) {
        var reg = /(?!^(\d+|[a-zA-Z]+|[~!@#$%^&*()_.]+)$)^[\w~!@#$%^&*()_.]{8,16}$/
        if (reg.test(value) === false) {
          callback(new Error(this.$t('Users.passwordFormat')))
        } else if (value.length < 8 || value.length > 16) {
          callback(new Error(this.$('passwordLength')))
        } else {
          callback()
        }
      } else if (!value) {
        callback(new Error(this.$t('Users.EnterNewPassword')))
      } else {
        callback()
      }
    }
    const pwdAgainCheck1 = (rule, value, callback) => {
      if (value || this.personalForm.password) {
        if(this.personalForm.password != this.personalForm.againPassword) {
          callback(new Error(this.$t('Users.TwoDifferentPasswordInputs')))
        } else {
          callback()
        }
      } if (!value) {
        callback(new Error(this.$('EnterNewPasswordAgain')))
      } else{
        callback()
      }
    }
    const pwdAgainCheck = (rule, value, callback) => {
      if (value.length < 1) {
        callback(new Error(this.$t('Users.DuplicatePasswordsCannotBeEmpty')))
      } else if (this.passwordForm.password != this.passwordForm.againPassword){
        callback(new Error(this.$t('Users.TwoDifferentPasswordInputs')))
      } else if (!value) {
        callback(new Error(this.$('EnterNewPasswordAgain')))
      } else{
        callback()
      }
    }
    const checkUsername = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Users.EnterYourUsername')))
      } else {
        callback()
      }
    }
    return {
      personalForm: {
        username: null,
        email: null,
        avatar: null,
        password: null,
        againPassword: null,
      },
      passwordForm: {
        username: null,
        password: null,
        againPassword: null,
      },
      personalRules: {
        username: [
          {required: true, trigger: ['blur'], validator: checkUsername}
        ],
        password: [
          { required: false, trigger: 'blur', validator: checkPassword }
        ],
        againPassword: [
          {required: false, trigger: ['blur'], validator: pwdAgainCheck1 }
        ]
      },
      passwordRules: {
        password: [
          { required: true, trigger: 'blur', validator: checkPassword }
        ],
        againPassword: [
          {required: true, trigger: ['blur'], validator: pwdAgainCheck }
        ]
      },
      showAvatarSelector: false
    }
  },
  components: {
    AvatarSelector,
  },
  created() {
    this.initData()
  },
	watch: {
  },
  mounted() {},
  methods: {
    initData () {
      this.personalForm.username = this.$store.state.user.name
      this.personalForm.email = this.$store.state.user.email
      this.personalForm.avatar = this.$store.state.user.avatar
      this.passwordForm.username = this.$store.state.user.name
      this.passwordForm.password = null
      this.passwordForm.againPassword = null
    },
    getUserInfo() {
      this.$store.dispatch("GetInfo").then((res) => {
      })
    },
    successMsg(message) {
      if (!message) {
        message = this.$t('Users.OperateSuccess');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    handleClick(e, link) {
      e.preventDefault()
    },
    showAvatarModal() {
      this.showAvatarSelector = true
    },
    avatarChange(icon) {
      this.personalForm.avatar = icon
      this.showAvatarSelector = false
    },
    personalFormSubmit() {
      this.$refs.personalForm.validate(valid => {
        if (valid) {
          let user = {
            username: this.personalForm.username,
            email: this.personalForm.email,
            avatar: this.personalForm.avatar,
            password: encrypt(this.personalForm.password)
          }
          updateUser(user).then(res => {
            if (user.password) {
              this.successMsg(this.$t('Users.LogInAgain'))
              setTimeout(() => {
                this.$store.dispatch('Logout').then(() => {
                  window.location.reload()
                })
              }, 500)
            } else {
              this.successMsg(this.$t('Users.InformationUpdated'))
            }
            this.getUserInfo()
          }).catch((err) => {
            this.$notification["error"]({
              message: this.$t('Users.FailedUpdateInformation'),
              description: ""
            })
          }).finally(() => {

          })
        }
      })
    },
    personalResetForm() {
      this.$refs.personalForm.resetFields()
      this.initData()
    },
    passwordFormSubmit() {
      this.$refs.passwordForm.validate(valid => {
        if (valid) {
          let user = {
            username: this.passwordForm.username,
            password: this.passwordForm.password,
          }
          user.password = encrypt(user.password)
          updateUser(user).then(res => {
            this.successMsg(this.$t('Users.PasswordUpdateSuccess'))
            setTimeout(() => {
              this.$store.dispatch('Logout').then(() => {
                window.location.reload()
              })
            }, 500)
          }).catch((err) => {
            this.$notification["error"]({
              message: this.$t('Users.PasswordUpdateFailed'),
              description: ""
            })
          }).finally(() => {

          })
        }
      })
    },
    passwordResetForm() {
      this.$refs.passwordForm.resetFields()
      this.initData()
    }
  },
}
</script>

<style lang="scss" scoped>

.personal::v-deep {
  .ant-anchor-ink::before {
    display: none;
  }

  .ant-list-item-meta {
    align-items: center;
  }

  .ant-list-item-meta-title {
    margin: 0;
  }

  .ant-anchor-link a {
    width: 100%;
    border-radius: 8px;
    color: #67748e !important;
    padding: 10px 16px;
    background-color: transparent;
    transition: background-color 0.3s ease-in;
  }

  .ant-anchor-link a svg {
    margin-right: 8px;
  }

  .folib-avatar {
    display: inline-block;
    width: 110px;
    height: 110px;
  }
}

</style>
