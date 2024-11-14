using UnityEngine;

public class PlayerDialogTrigger : MonoBehaviour
{

    private bool CanStartDialog = false;
    private Yarn.Unity.DialogueRunner dialog_runner;
    private Yarn.Unity.LineView line_view;
    // Start is called once before the first execution of Update after the MonoBehaviour is created
    void Start()
    {
        
    }

    private void Awake()
    {
        dialog_runner = GameObject.Find("Dialogue System").GetComponent<Yarn.Unity.DialogueRunner>();
        line_view = GameObject.Find("Line View").GetComponent<Yarn.Unity.LineView>();
    }

    // Update is called once per frame
    void Update()
    {
        if(CanStartDialog)
        {
            if(Input.GetKeyDown(KeyCode.E))
            {
                if(dialog_runner != null)
                {
                    dialog_runner.StartDialogue("Start");
                }

                if(line_view != null)
                {
                    line_view.UserRequestedViewAdvancement();
                }
            }

            
        }
    }

    private void OnTriggerEnter(Collider other)
    {
        if (other.tag == "NPC")
        {
           CanStartDialog = true;
           print("hit npc");
        }
    }

    private void OnTriggerExit(Collider other)
    {
        if (other.tag == "NPC")
        {
            CanStartDialog = false;
            print("exited npc");
        }
    }

}
