using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;


[System.Serializable]
public struct SkillContainer{
    public VFX_BaseSkill PF_Skill;
    public float secToPlayEffect;
    public string animatoinToPlay;
    public float secToStartAnimation;
    public float secToStopAnimation;
    public float vfxDistanceToSpawn;
}

public class SkillTester : MonoBehaviour
{
    public static int dissolveID = Shader.PropertyToID("_Dissolve");
    public SkillContainer[] skillToTest;
    private int skillIndex;

    [SerializeField]
    private Text skillName;
    [SerializeField]
    private Material[] materials;
    private Animator anim;
    private bool isCastingSkill;
    public string dissolveAnimationName;
    [SerializeField]
    private bool isDissolveing;
    private float dissolveAmount;

    void Awake(){
        anim = transform.GetChild(0).GetComponent<Animator>();

    }
    void Start()
    {
        skillIndex = 0;
        foreach (var mat in materials)
        {
            mat.SetFloat(dissolveID,0);
        }
    }

    void Update()
    {
        if (Input.GetKeyDown(KeyCode.Space)) {
            ActivateSkill();
        }
        if (Input.GetKeyDown(KeyCode.F)) {
            isDissolveing = !isDissolveing;
        }
        if(Input.GetKeyDown(KeyCode.Q) && skillIndex > 0){
            skillIndex -= 1;
        }
         if(Input.GetKeyDown(KeyCode.E) && skillIndex < skillToTest.Length - 1){
            skillIndex += 1;
        }

        PlayDissolveEffect(isDissolveing);

        skillName.text = skillToTest[skillIndex].PF_Skill.name;
        anim.SetBool("isCasting",isCastingSkill);
        anim.SetBool("isDissolveing", isDissolveing);
    }
    
    private void ActivateSkill(){
        isCastingSkill = true;
        Vector3 vfxPos = (transform.forward * skillToTest[skillIndex].vfxDistanceToSpawn) + new Vector3(0,0.1f,0);
        VFX_BaseSkill currentVFX = Instantiate(skillToTest[skillIndex].PF_Skill,vfxPos,transform.rotation);
        StartCoroutine(ActivateSkillCoroutine(currentVFX));
    }

    private IEnumerator ActivateSkillCoroutine(VFX_BaseSkill _currentVFX){
        SkillContainer currentSkill = skillToTest[skillIndex];
        yield return new WaitForSeconds(currentSkill.secToStartAnimation);
        anim.Play(currentSkill.animatoinToPlay,0,0);
        yield return new WaitForSeconds(currentSkill.secToPlayEffect);
        _currentVFX.LaunchSkill();
        yield return new WaitForSeconds(currentSkill.secToStopAnimation);
        isCastingSkill = false;
    }

    private void PlayDissolveEffect(bool _b) {

        if (_b)
            anim.Play(dissolveAnimationName,0);

        StartCoroutine(DissolveEffectCoroutine(_b));
    }

    private IEnumerator DissolveEffectCoroutine(bool _b) {
        if (_b)
        {
            while (dissolveAmount < 1)
            {
                dissolveAmount += 0.01f * Time.deltaTime;
                foreach (var mat in materials)
                {
                    mat.SetFloat(dissolveID, dissolveAmount);
                }
                yield return null;
            }
        }
        else {
            while (dissolveAmount > 0)
            {
                dissolveAmount -= 0.01f * Time.deltaTime;
                foreach (var mat in materials)
                {
                    mat.SetFloat(dissolveID, dissolveAmount);
                }
                yield return null;
            }
        }
    }
}
