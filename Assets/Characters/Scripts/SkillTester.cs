using System.Collections;
using System.Collections.Generic;
using UnityEngine;


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
    public SkillContainer[] skillToTest;
    private int skillIndex;
    [SerializeField]
    private Transform skillPoint;
    private Animator anim;
    private bool isCastingSkill;

    void Awake(){
        anim = transform.GetChild(0).GetComponent<Animator>();

    }
    void Start()
    {
        skillIndex = 0;
    }

    void Update()
    {
        if(Input.GetKeyDown(KeyCode.Space)){
           ActivateSkill();
        }
        if(Input.GetKeyDown(KeyCode.Q) && skillIndex > 0){
            skillIndex -= 1;
        }
         if(Input.GetKeyDown(KeyCode.E) && skillIndex < skillToTest.Length - 1){
            skillIndex += 1;
        }

        anim.SetBool("isCasting",isCastingSkill);
    }
    
    void ActivateSkill(){
        isCastingSkill = true;
        Vector3 vfxPos = (transform.forward * skillToTest[skillIndex].vfxDistanceToSpawn) + new Vector3(0,0.1f,0);
        VFX_BaseSkill currentVFX = Instantiate(skillToTest[skillIndex].PF_Skill,vfxPos,transform.rotation);
        StartCoroutine(ActivateSkillCoroutine(currentVFX));
    }

    IEnumerator ActivateSkillCoroutine(VFX_BaseSkill _currentVFX){
        SkillContainer currentSkill = skillToTest[skillIndex];
        yield return new WaitForSeconds(currentSkill.secToStartAnimation);
        anim.Play(currentSkill.animatoinToPlay,0,0);
        yield return new WaitForSeconds(currentSkill.secToPlayEffect);
        _currentVFX.LaunchSkill();
        yield return new WaitForSeconds(currentSkill.secToStopAnimation);
        isCastingSkill = false;
    }
}
